use super::{InferContext, Type};
use sealed::Sealed;

pub(crate) mod call;
pub(crate) mod implicit_dunder_call;
pub(crate) mod lookup;

mod sealed;

pub(crate) trait TypeOperationError<'db>: sealed::Sealed {
    type ErrorContext;
    fn into_type(
        self,
        inference_context: &InferContext,
        error_context: &Self::ErrorContext,
    ) -> Type<'db>;
}

pub(crate) trait TypeOperationResult<'db>: sealed::Sealed {
    type ErrorContext;
    fn unwrap_with_diagnostic(
        self,
        inference_context: &InferContext,
        error_context: &Self::ErrorContext,
    ) -> Type<'db>;
}

impl<'db, E> Sealed for Result<Type<'db>, E> where E: TypeOperationError<'db> {}

impl<'db, E> TypeOperationResult<'db> for Result<Type<'db>, E>
where
    E: TypeOperationError<'db>,
{
    type ErrorContext = E::ErrorContext;

    fn unwrap_with_diagnostic(
        self,
        inference_context: &InferContext,
        error_context: &Self::ErrorContext,
    ) -> Type<'db> {
        match self {
            Ok(ty) => ty,
            Err(err) => err.into_type(inference_context, error_context),
        }
    }
}
