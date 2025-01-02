//! Generic error

use crate::types::Type;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum DunderLookupError {
    Unbound,
    PossiblyUnbound,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum DunderCallErrorKind {
    NotCallable,
    PossiblyCallable,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct DunderCallError<'db> {
    pub(crate) kind: DunderCallErrorKind,
    pub(crate) dunder_call_type: Type<'db>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ImplicitDunderCallError<'db> {
    pub(crate) dunder_lookup_error: Option<DunderLookupError>,
    pub(crate) dunder_callability: Option<DunderCallError<'db>>,
    pub(crate) fallback_return_type: Type<'db>,
}

pub(crate) type ImplicitDunderCallResult<'db> = Result<Type<'db>, ImplicitDunderCallError<'db>>;
