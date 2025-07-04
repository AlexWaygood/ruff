use ruff_macros::{ViolationMetadata, derive_message_formats};
use ruff_python_ast as ast;
use ruff_python_semantic::Modules;
use ruff_text_size::Ranged;

use crate::checkers::ast::Checker;
use crate::importer::ImportRequest;
use crate::{Edit, Fix, FixAvailability, Violation};

/// ## What it does
/// Checks for direct instantiation of `logging.Logger`, as opposed to using
/// `logging.getLogger()`.
///
/// ## Why is this bad?
/// The [Logger Objects] documentation states that:
///
/// > Note that Loggers should NEVER be instantiated directly, but always
/// > through the module-level function `logging.getLogger(name)`.
///
/// If a logger is directly instantiated, it won't be added to the logger
/// tree, and will bypass all configuration. Messages logged to it will
/// only be sent to the "handler of last resort", skipping any filtering
/// or formatting.
///
/// ## Example
/// ```python
/// import logging
///
/// logger = logging.Logger(__name__)
/// ```
///
/// Use instead:
/// ```python
/// import logging
///
/// logger = logging.getLogger(__name__)
/// ```
///
/// ## Fix safety
/// This fix is always unsafe, as changing from `Logger` to `getLogger`
/// changes program behavior by will adding the logger to the logging tree.
///
/// [Logger Objects]: https://docs.python.org/3/library/logging.html#logger-objects
#[derive(ViolationMetadata)]
pub(crate) struct DirectLoggerInstantiation;

impl Violation for DirectLoggerInstantiation {
    const FIX_AVAILABILITY: FixAvailability = FixAvailability::Sometimes;

    #[derive_message_formats]
    fn message(&self) -> String {
        "Use `logging.getLogger()` to instantiate loggers".to_string()
    }

    fn fix_title(&self) -> Option<String> {
        Some("Replace with `logging.getLogger()`".to_string())
    }
}

/// LOG001
pub(crate) fn direct_logger_instantiation(checker: &Checker, call: &ast::ExprCall) {
    if !checker.semantic().seen_module(Modules::LOGGING) {
        return;
    }

    if checker
        .semantic()
        .resolve_qualified_name(call.func.as_ref())
        .is_some_and(|qualified_name| matches!(qualified_name.segments(), ["logging", "Logger"]))
    {
        let mut diagnostic =
            checker.report_diagnostic(DirectLoggerInstantiation, call.func.range());
        diagnostic.try_set_fix(|| {
            let (import_edit, binding) = checker.importer().get_or_import_symbol(
                &ImportRequest::import("logging", "getLogger"),
                call.func.start(),
                checker.semantic(),
            )?;
            let reference_edit = Edit::range_replacement(binding, call.func.range());
            Ok(Fix::unsafe_edits(import_edit, [reference_edit]))
        });
    }
}
