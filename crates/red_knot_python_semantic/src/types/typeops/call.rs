use std::ops::Deref;

use ruff_db::diagnostic::{DiagnosticId, Severity};
use ruff_python_ast as ast;

use crate::types::{diagnostic::CALL_NON_CALLABLE, Type};

use super::implicit_dunder_call::{
    DunderCallError, DunderCallErrorKind, DunderLookupError, ImplicitDunderCallError,
};
use super::{InferContext, Sealed, TypeOperationError};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct CallError<'db>(ImplicitDunderCallError<'db>);

impl<'db> Deref for CallError<'db> {
    type Target = ImplicitDunderCallError<'db>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Sealed for CallError<'_> {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum SpecialCasedFunction<'db> {
    RevealType { revealed: Type<'db> },
}

#[derive(Debug, PartialEq)]
pub(crate) struct CallErrorContext<'db> {
    pub(crate) called_type: Type<'db>,
    pub(crate) node: &'db ast::ExprCall,
    pub(crate) special_cased_function: Option<SpecialCasedFunction<'db>>,
}

impl<'db> CallError<'db> {
    fn unbound_dunder_call(inference_context: &InferContext, error_context: &CallErrorContext) {
        let db = inference_context.db();

        inference_context.report_lint(
            &CALL_NON_CALLABLE,
             error_context.node.into(),
             format_args!(
                "Object of type `{called_type}` is not callable as it does not have a `__call__` method",
                called_type = error_context.called_type.display(db),
            ),
        );
    }

    fn possibly_unbound_dunder_call(
        inference_context: &InferContext,
        error_context: &CallErrorContext,
    ) {
        let db = inference_context.db();
        inference_context.report_lint(
            &CALL_NON_CALLABLE,
             error_context.node.into(),
             format_args!(
                "Object of type {called_type} may not be callable as its `__call__` method may be unbound",
                called_type = error_context.called_type.display(db),
            ),
        );
    }

    fn dunder_call_method_not_callable(
        inference_context: &InferContext,
        error_context: &CallErrorContext,
        dunder_call_type: Type<'db>,
    ) {
        let db = inference_context.db();
        inference_context.report_lint(
            &CALL_NON_CALLABLE,
             error_context.node.into(),
             format_args!(
                "Object of type {called_type} is not callable as its `__call__` method has type {dunder_call_type}, which is not callable",
                called_type = error_context.called_type.display(db),
                dunder_call_type = dunder_call_type.display(db),
            ),
        );
    }

    fn dunder_call_method_possibly_not_callable(
        inference_context: &InferContext,
        error_context: &CallErrorContext,
        dunder_call_type: Type<'db>,
    ) {
        let db = inference_context.db();
        inference_context.report_lint(
            &CALL_NON_CALLABLE,
             error_context.node.into(),
             format_args!(
                "Object of type {called_type} may not be callable as its `__call__` method has type {dunder_call_type}, which may not be callable",
                called_type = error_context.called_type.display(db),
                dunder_call_type = dunder_call_type.display(db),
            ),
        );
    }

    fn special_cased_function(
        inference_context: &InferContext,
        error_context: &CallErrorContext,
        special_cased_function: SpecialCasedFunction<'db>,
    ) {
        match special_cased_function {
            SpecialCasedFunction::RevealType { revealed } => {
                inference_context.report_diagnostic(
                    error_context.node.into(),
                    DiagnosticId::RevealedType,
                    Severity::Info,
                    format_args!(
                        "Revealed type is `{}`",
                        revealed.display(inference_context.db())
                    ),
                );
            }
        }
    }
}

impl<'db> TypeOperationError<'db> for CallError<'db> {
    type ErrorContext = CallErrorContext<'db>;

    fn into_type(
        self,
        inference_context: &InferContext,
        error_context: &Self::ErrorContext,
    ) -> Type<'db> {
        let CallError(ImplicitDunderCallError {
            dunder_lookup_error,
            dunder_callability,
            fallback_return_type,
        }) = self;

        if let Some(error) = dunder_lookup_error {
            match error {
                DunderLookupError::Unbound => {
                    Self::unbound_dunder_call(inference_context, error_context);
                }
                DunderLookupError::PossiblyUnbound => {
                    Self::possibly_unbound_dunder_call(inference_context, error_context);
                }
            }
        }

        if let Some(DunderCallError {
            kind,
            dunder_call_type,
        }) = dunder_callability
        {
            match kind {
                DunderCallErrorKind::NotCallable => {
                    Self::dunder_call_method_not_callable(
                        inference_context,
                        error_context,
                        dunder_call_type,
                    );
                }
                DunderCallErrorKind::PossiblyCallable => {
                    Self::dunder_call_method_possibly_not_callable(
                        inference_context,
                        error_context,
                        dunder_call_type,
                    );
                }
            }
        }

        if let Some(special_cased_function) = error_context.special_cased_function {
            Self::special_cased_function(inference_context, error_context, special_cased_function);
        }

        fallback_return_type
    }
}
