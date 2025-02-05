use ruff_python_ast::{self as ast, Stmt};

use ruff_diagnostics::{Diagnostic, Violation};
use ruff_macros::{derive_message_formats, ViolationMetadata};
use ruff_python_semantic::analyze::typing::{is_immutable_annotation, is_mutable_expr};
use ruff_text_size::Ranged;

use crate::checkers::ast::Checker;
use crate::rules::ruff::rules::helpers::{
    dataclass_kind, has_default_copy_semantics, is_class_var_annotation, is_final_annotation,
    is_special_attribute,
};

/// ## What it does
/// Checks for mutable default values in class attributes.
///
/// ## Why is this bad?
/// Mutable default values share state across all instances of the class,
/// while not being obvious. This can lead to bugs when the attributes are
/// changed in one instance, as those changes will unexpectedly affect all
/// other instances.
///
/// When mutable values are intended, they should be annotated with
/// `typing.ClassVar`. When mutability is not required, values should be
/// immutable types, like `tuple` or `frozenset`.
///
/// For mutable variables, prefer to initialize them in `__init__`.
///
/// ## Examples
///
/// Using `ClassVar` and imutable types:
///
/// ```python
/// class A:
///     mutable_default: list[int] = []
///     immutable_default: list[int] = []
/// ```
///
/// Use instead:
///
/// ```python
/// from typing import ClassVar
///
///
/// class A:
///     mutable_default: ClassVar[list[int]] = []
///     immutable_default: tuple[int, ...] = ()
/// ```
///
/// Using instance variables instead of class variables:
///
/// ```python
/// class A:
///     instance_dict: dict[str, str] = {"key": "value"}
/// ```
///
/// Use instead:
///
/// ```python
/// class A:
///     instance_dict: ClassVar[dict[str, str]]
///
///     def __init__(self) -> None:
///         self.instance_dict: dict[str, str] = {"key": "value"}
/// ```
///
/// In cases where memory efficiency is a priority, `MappingProxyType`
/// can be used to create immutable dictionaries that are shared between
/// instances.
#[derive(ViolationMetadata)]
pub(crate) struct MutableClassDefault;

impl Violation for MutableClassDefault {
    #[derive_message_formats]
    fn message(&self) -> String {
        "Mutable class attributes should be annotated with `typing.ClassVar`".to_string()
    }
}

/// RUF012
pub(crate) fn mutable_class_default(
    checker: &Checker,
    class_def: &ast::StmtClassDef,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for statement in &class_def.body {
        match statement {
            Stmt::AnnAssign(ast::StmtAnnAssign {
                annotation,
                target,
                value: Some(value),
                ..
            }) => {
                if !is_special_attribute(target)
                    && is_mutable_expr(value, checker.semantic())
                    && !is_class_var_annotation(annotation, checker.semantic())
                    && !is_final_annotation(annotation, checker.semantic())
                    && !is_immutable_annotation(annotation, checker.semantic(), &[])
                {
                    if dataclass_kind(class_def, checker.semantic()).is_some() {
                        continue;
                    }

                    // Avoid, e.g., Pydantic and msgspec models, which end up copying defaults on instance creation.
                    if has_default_copy_semantics(class_def, checker.semantic()) {
                        return;
                    }

                    diagnostics.push(Diagnostic::new(MutableClassDefault, value.range()));
                }
            }
            Stmt::Assign(ast::StmtAssign { value, targets, .. }) => {
                if !targets.iter().all(is_special_attribute)
                    && is_mutable_expr(value, checker.semantic())
                {
                    // Avoid, e.g., Pydantic and msgspec models, which end up copying defaults on instance creation.
                    if has_default_copy_semantics(class_def, checker.semantic()) {
                        return;
                    }

                    diagnostics.push(Diagnostic::new(MutableClassDefault, value.range()));
                }
            }
            _ => (),
        }
    }
}
