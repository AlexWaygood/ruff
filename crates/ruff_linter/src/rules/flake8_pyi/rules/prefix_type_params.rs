use std::fmt;

use ruff_diagnostics::{Diagnostic, Violation};
use ruff_macros::{derive_message_formats, violation};
use ruff_python_ast::{self as ast, Expr};
use ruff_text_size::Ranged;

use crate::checkers::ast::Checker;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) enum VarKind {
    TypeVar,
    ParamSpec,
    TypeVarTuple,
}

impl fmt::Display for VarKind {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(match self {
            VarKind::TypeVar => "TypeVar",
            VarKind::ParamSpec => "ParamSpec",
            VarKind::TypeVarTuple => "TypeVarTuple",
        })
    }
}

/// ## What it does
/// Checks that type `TypeVar`s, `ParamSpec`s, and `TypeVarTuple`s in stubs
/// have names prefixed with `_`.
///
/// ## Why is this bad?
/// Prefixing type parameters with `_` avoids accidentally exposing names
/// internal to the stub.
///
/// ## Example
/// ```python
/// from typing import TypeVar
///
/// T = TypeVar("T")
/// ```
///
/// Use instead:
/// ```python
/// from typing import TypeVar
///
/// _T = TypeVar("_T")
/// ```
#[violation]
pub struct UnprefixedTypeParam {
    kind: VarKind,
}

impl Violation for UnprefixedTypeParam {
    #[derive_message_formats]
    fn message(&self) -> String {
        let UnprefixedTypeParam { kind } = self;
        format!("Name of private `{kind}` must start with `_`")
    }
}

/// PYI001
pub(crate) fn prefix_type_params(checker: &mut Checker, value: &Expr, targets: &[Expr]) {
    // If the typing modules were never imported, we'll never match below.
    if !checker.semantic().seen_typing() {
        return;
    }

    let [Expr::Name(ast::ExprName { id, .. })] = targets else {
        return;
    };

    if id.starts_with('_') {
        return;
    }

    let Expr::Call(ast::ExprCall { func, .. }) = value else {
        return;
    };

    let semantic = checker.semantic();

    let Some(qualified_name) = semantic.resolve_qualified_name(func) else {
        return;
    };

    let kind = {
        if semantic.match_typing_qualified_name(&qualified_name, "ParamSpec") {
            VarKind::ParamSpec
        } else if semantic.match_typing_qualified_name(&qualified_name, "TypeVar") {
            VarKind::TypeVar
        } else if semantic.match_typing_qualified_name(&qualified_name, "TypeVarTuple") {
            VarKind::TypeVarTuple
        } else {
            return;
        }
    };

    checker
        .diagnostics
        .push(Diagnostic::new(UnprefixedTypeParam { kind }, value.range()));
}
