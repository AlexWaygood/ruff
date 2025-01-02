use ruff_python_ast as ast;

use crate::db::Db;
use crate::types::diagnostic::{
    report_possibly_unresolved_reference, report_unresolved_reference, POSSIBLY_UNBOUND_ATTRIBUTE,
    UNRESOLVED_ATTRIBUTE,
};
use crate::types::{Type, UnionType};

use super::{Sealed, TypeOperationError};

pub(crate) type LookupResult<'db> = Result<Type<'db>, LookupError<'db>>;

/// An error that occurs due to a symbol being looked up,
/// either explicitly or implicitly.
///
/// Symbol lookups can happen explicitly. Here, for example,
/// the `x` symbol is looked up explicitly as a name
/// in the `print(x)` call:
///
/// ```python
/// print(x)
/// ```
///
/// And here, the `x` symbol is also looked up explicitly (but
/// this time, it is looked up as an attribute on the `C` symbol):
///
/// ```python
/// print(C.x)
/// ```
///
/// But symbol lookups can also happen implicitly. Here, for example,
/// the `__len__` symbol is looked up implicitly as an attribute
/// on `type(x)` as a result of the `len(x)` call:
///
/// ```python
/// len(x)
/// ```
///
/// Whether and how it is appropriate to emit a diagnostic as a result
/// of a failing lookup depends on the context in which the lookup
/// occurs.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum LookupError<'db> {
    Unresolved,
    PossiblyUnresolved { type_when_available: Type<'db> },
}

impl Sealed for LookupError<'_> {}

#[derive(Debug, PartialEq)]
pub(crate) enum LookupErrorContext<'db> {
    /// A name looked up in a certain scope, e.g. `x` in `print(x)`
    Name { node: &'db ast::ExprName },

    /// An attribute looked up on another object
    Attribute {
        node: &'db ast::ExprAttribute,
        accessed_on: Type<'db>,
    },
}

impl<'db> TypeOperationError<'db> for LookupError<'db> {
    type ErrorContext = LookupErrorContext<'db>;

    fn into_type(
        self,
        inference_context: &crate::types::InferContext,
        error_context: &Self::ErrorContext,
    ) -> Type<'db> {
        match (error_context, self) {
            (LookupErrorContext::Name { node }, LookupError::Unresolved) => {
                report_unresolved_reference(inference_context, node);
                Type::Unknown
            }
            (
                LookupErrorContext::Name { node },
                LookupError::PossiblyUnresolved {
                    type_when_available,
                },
            ) => {
                report_possibly_unresolved_reference(inference_context, node);
                type_when_available
            }
            (LookupErrorContext::Attribute { node, accessed_on }, LookupError::Unresolved) => {
                inference_context.report_lint(
                    &UNRESOLVED_ATTRIBUTE,
                    (*node).into(),
                    format_args!(
                        "Object of type `{accessed_on}` has no attribute `{attribute}`",
                        accessed_on = accessed_on.display(inference_context.db()),
                        attribute = node.attr,
                    ),
                );
                Type::Unknown
            }
            (
                LookupErrorContext::Attribute { node, accessed_on },
                LookupError::PossiblyUnresolved {
                    type_when_available,
                },
            ) => {
                inference_context.report_lint(
                    &POSSIBLY_UNBOUND_ATTRIBUTE,
                    (*node).into(),
                    format_args!(
                        "Attribute `{attribute}` may be unbound on object of type `{accessed_on}`",
                        attribute = node.attr,
                        accessed_on = accessed_on.display(inference_context.db()),
                    ),
                );
                type_when_available
            }
        }
    }
}

pub(crate) trait LookupResultExt<'db>: Sealed {
    fn unresolved() -> Self;
    fn possibly_unresolved(type_when_available: Type<'db>) -> Self;
    fn or_fall_back_to(self, db: &'db dyn Db, other: Self) -> Self;
}

impl<'db> LookupResultExt<'db> for Result<Type<'db>, LookupError<'db>> {
    fn possibly_unresolved(type_when_available: Type<'db>) -> Self {
        Self::Err(LookupError::PossiblyUnresolved { type_when_available })
    }

    fn unresolved() -> Self {
        Self::Err(LookupError::Unresolved)
    }

    fn or_fall_back_to(self, db: &'db dyn Db, other: Self) -> Self {
        match (self, other) {
            (Ok(_), _) => self,
            (Err(LookupError::Unresolved), _) => other,
            (Err(LookupError::PossiblyUnresolved { .. }), Err(LookupError::Unresolved)) => self,
            (
                Err(LookupError::PossiblyUnresolved {
                    type_when_available: first_type,
                }),
                Err(LookupError::PossiblyUnresolved {
                    type_when_available: second_type,
                }),
            ) => Err(LookupError::PossiblyUnresolved {
                type_when_available: UnionType::from_elements(db, [first_type, second_type]),
            }),
            (
                Err(LookupError::PossiblyUnresolved {
                    type_when_available: first_type,
                }),
                Ok(second_type),
            ) => Ok(UnionType::from_elements(db, [first_type, second_type])),
        }
    }
}
