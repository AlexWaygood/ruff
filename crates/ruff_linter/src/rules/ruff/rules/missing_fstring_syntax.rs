use ruff_diagnostics::{AlwaysFixableViolation, Diagnostic, Edit, Fix};
use ruff_macros::{derive_message_formats, violation};
use ruff_python_ast as ast;
use ruff_python_ast::visitor::Visitor;
use ruff_python_literal::format::FormatSpec;
use ruff_python_parser::parse_expression;
use ruff_python_semantic::analyze::logging;
use ruff_python_semantic::{ScopeKind, SemanticModel};
use ruff_source_file::Locator;
use ruff_text_size::{Ranged, TextRange};

use memchr::memchr2_iter;
use rustc_hash::FxHashSet;

use crate::checkers::ast::Checker;

/// ## What it does
/// Searches for strings that look like they were meant to be f-strings, but are missing an `f` prefix.
///
/// ## Why is this bad?
/// Expressions inside curly braces are only evaluated if the string has an `f` prefix.
///
/// ## Details
///
/// There are many possible string literals which are not meant to be f-strings
/// despite containing f-string-like syntax. As such, this lint ignores all strings
/// where one of the following conditions applies:
///
/// 1. The string is a standalone expression. For example, the rule ignores all docstrings.
/// 2. The string is part of a function call with argument names that match at least one variable
///    (for example: `format("Message: {value}", value="Hello World")`)
/// 3. The string (or a parent expression of the string) has a direct method call on it
///    (for example: `"{value}".format(...)`)
/// 4. The string has no `{...}` expression sections, or uses invalid f-string syntax.
/// 5. The string references variables that are not in scope, or it doesn't capture variables at all.
/// 6. Any format specifiers in the potential f-string are invalid.
///
/// ## Example
///
/// ```python
/// name = "Sarah"
/// day_of_week = "Tuesday"
/// print("Hello {name}! It is {day_of_week} today!")
/// ```
///
/// Use instead:
/// ```python
/// name = "Sarah"
/// day_of_week = "Tuesday"
/// print(f"Hello {name}! It is {day_of_week} today!")
/// ```
#[violation]
pub struct MissingFStringSyntax;

impl AlwaysFixableViolation for MissingFStringSyntax {
    #[derive_message_formats]
    fn message(&self) -> String {
        format!(r#"Possible f-string without an `f` prefix"#)
    }

    fn fix_title(&self) -> String {
        "Add `f` prefix".into()
    }
}

/// RUF027
pub(crate) fn missing_fstring_syntax(checker: &mut Checker, literal: &ast::StringLiteral) {
    let semantic = checker.semantic();

    // we want to avoid statement expressions that are just a string literal.
    // there's no reason to have standalone f-strings and this lets us avoid docstrings too
    if let ast::Stmt::Expr(ast::StmtExpr { value, .. }) = semantic.current_statement() {
        match value.as_ref() {
            ast::Expr::StringLiteral(_) | ast::Expr::FString(_) => return,
            _ => {}
        }
    }

    let logger_objects = &checker.settings.logger_objects;

    // We also want to avoid expressions that are intended to be translated.
    // And we should avoid "foo {}" strings passed to loggers as well,
    // since the `logging` module lazily evaluates these strings:
    // https://docs.python.org/3/howto/logging-cookbook.html#using-particular-formatting-styles-throughout-your-application
    if semantic
        .current_expressions()
        .any(|expr| is_gettext(expr, semantic) || is_logger_call(expr, semantic, logger_objects))
    {
        return;
    }

    let variable_name = match semantic.current_statement() {
        ast::Stmt::AnnAssign(ast::StmtAnnAssign { target, .. }) => Some(&**target),
        ast::Stmt::Assign(ast::StmtAssign { targets, .. }) => match targets.as_slice() {
            [target] => Some(target),
            _ => None,
        },
        _ => None,
    };

    if let Some(ast::Expr::Name(ast::ExprName {
        id: variable_name, ..
    })) = variable_name
    {
        if variable_could_be_template_string(variable_name, semantic, logger_objects) {
            return;
        }
    };

    if should_be_fstring(literal, checker.locator(), semantic) {
        let diagnostic = Diagnostic::new(MissingFStringSyntax, literal.range())
            .with_fix(fix_fstring_syntax(literal.range()));
        checker.diagnostics.push(diagnostic);
    }
}

/// Return `true` if any `logging` or `gettext` calls inside the current scope
/// are passed the variable to which the string literal we're linting was assigned.
///
/// E.g. return `true` for something like the following:
/// ```python
/// import logging
///
/// def foo():
///     template_string = "{foo}"
///     logging.log(template_string)
/// ```
///
/// Also return true if `.format()` is called on the variable
/// anywhere in the same scope, e.g.
/// ```python
/// def foo():
///     bar = {x}
///     bar.format(42)
/// ```
fn variable_could_be_template_string(
    variable_name: &ast::name::Name,
    semantic: &SemanticModel,
    logger_objects: &[String],
) -> bool {
    let current_scope_body = match semantic.current_scope().kind {
        ScopeKind::Class(ast::StmtClassDef { body, .. }) => body,
        ScopeKind::Function(ast::StmtFunctionDef { body, .. }) => body,
        _ => return false,
    };
    let mut searcher = StringTemplateSearcher {
        variable_name,
        semantic,
        logger_objects,
        found_template_string: false,
    };
    searcher.visit_body(current_scope_body);
    searcher.found_template_string
}

/// AST visitor that searches the body of a given scope for call expressions
/// that match one of the following two criteria:
/// (1) - They're passed a variable with a specific name, AND
///     - They look like they could be `logging` or `gettext` calls
///       (both of which accept template strings), OR
/// (2) - The function being called is `${variable_name}.format(...)`
struct StringTemplateSearcher<'a> {
    variable_name: &'a ast::name::Name,
    semantic: &'a SemanticModel<'a>,
    logger_objects: &'a [String],
    found_template_string: bool,
}

impl Visitor<'_> for StringTemplateSearcher<'_> {
    fn visit_expr(&mut self, expr: &ast::Expr) {
        if self.found_template_string {
            return;
        }

        if let ast::Expr::Call(ast::ExprCall {
            func,
            arguments: ast::Arguments { args, keywords, .. },
            ..
        }) = expr
        {
            if let ast::Expr::Attribute(ast::ExprAttribute { value, attr, .. }) = &**func {
                if attr == "format" {
                    if let ast::Expr::Name(ast::ExprName { id, .. }) = &**value {
                        if id == self.variable_name {
                            self.found_template_string = true;
                            return;
                        }
                    }
                }
            }
            for name_passed in args
                .iter()
                .chain(keywords.iter().map(|keyword| &keyword.value))
                .filter_map(ast::Expr::as_name_expr)
            {
                if &name_passed.id != self.variable_name {
                    continue;
                }
                if logging::is_logger_candidate(func, self.semantic, self.logger_objects) {
                    self.found_template_string = true;
                    return;
                }
                if is_gettext(func, self.semantic) {
                    self.found_template_string = true;
                    return;
                }
            }
        }

        ast::visitor::walk_expr(self, expr);
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        if self.found_template_string {
            return;
        }

        // Don't iterate into sub-scopes, just look at the scope we're given.
        // Also don't both with imports, since imports can't contain call expressions
        if matches!(
            stmt,
            ast::Stmt::FunctionDef(_)
                | ast::Stmt::ClassDef(_)
                | ast::Stmt::Import(_)
                | ast::Stmt::ImportFrom(_)
        ) {
            return;
        }

        ast::visitor::walk_stmt(self, stmt);
    }

    fn visit_body(&mut self, body: &[ruff_python_ast::Stmt]) {
        for stmt in body {
            if self.found_template_string {
                return;
            }
            self.visit_stmt(stmt);
        }
    }
}

fn is_logger_call(expr: &ast::Expr, semantic: &SemanticModel, logger_objects: &[String]) -> bool {
    let ast::Expr::Call(ast::ExprCall { func, .. }) = expr else {
        return false;
    };
    logging::is_logger_candidate(func, semantic, logger_objects)
}

/// Returns `true` if an expression appears to be a `gettext` call.
///
/// We want to avoid statement expressions and assignments related to aliases
/// of the gettext API.
///
/// See <https://docs.python.org/3/library/gettext.html> for details. When one
/// uses `_` to mark a string for translation, the tools look for these markers
/// and replace the original string with its translated counterpart. If the
/// string contains variable placeholders or formatting, it can complicate the
/// translation process, lead to errors or incorrect translations.
fn is_gettext(expr: &ast::Expr, semantic: &SemanticModel) -> bool {
    let ast::Expr::Call(ast::ExprCall { func, .. }) = expr else {
        return false;
    };

    let short_circuit = match func.as_ref() {
        ast::Expr::Name(ast::ExprName { id, .. }) => {
            matches!(id.as_str(), "gettext" | "ngettext" | "_")
        }
        ast::Expr::Attribute(ast::ExprAttribute { attr, .. }) => {
            matches!(attr.as_str(), "gettext" | "ngettext")
        }
        _ => false,
    };

    if short_circuit {
        return true;
    }

    semantic
        .resolve_qualified_name(func)
        .is_some_and(|qualified_name| {
            matches!(
                qualified_name.segments(),
                ["gettext", "gettext" | "ngettext"] | ["builtins", "_"]
            )
        })
}

/// Returns `true` if `literal` is likely an f-string with a missing `f` prefix.
/// See [`MissingFStringSyntax`] for the validation criteria.
fn should_be_fstring(
    literal: &ast::StringLiteral,
    locator: &Locator,
    semantic: &SemanticModel,
) -> bool {
    if !has_brackets(&literal.value) {
        return false;
    }

    let fstring_expr = format!("f{}", locator.slice(literal));
    let Ok(parsed) = parse_expression(&fstring_expr) else {
        return false;
    };

    // Note: Range offsets for `value` are based on `fstring_expr`
    let ast::Expr::FString(ast::ExprFString { value, .. }) = parsed.expr() else {
        return false;
    };

    let mut arg_names = FxHashSet::default();
    let mut last_expr: Option<&ast::Expr> = None;
    for expr in semantic.current_expressions() {
        match expr {
            ast::Expr::Call(ast::ExprCall {
                arguments: ast::Arguments { keywords, args, .. },
                func,
                ..
            }) => {
                if let ast::Expr::Attribute(ast::ExprAttribute { value, .. }) = func.as_ref() {
                    match value.as_ref() {
                        // if the first part of the attribute is the string literal,
                        // we want to ignore this literal from the lint.
                        // for example: `"{x}".some_method(...)`
                        ast::Expr::StringLiteral(expr_literal)
                            if expr_literal.value.as_slice().contains(literal) =>
                        {
                            return false;
                        }
                        // if the first part of the attribute was the expression we
                        // just went over in the last iteration, then we also want to pass
                        // this over in the lint.
                        // for example: `some_func("{x}").some_method(...)`
                        value if last_expr == Some(value) => {
                            return false;
                        }
                        _ => {}
                    }
                }
                for keyword in &**keywords {
                    if let Some(ident) = keyword.arg.as_ref() {
                        arg_names.insert(ident.as_str());
                    }
                }
                for arg in &**args {
                    if let ast::Expr::Name(ast::ExprName { id, .. }) = arg {
                        arg_names.insert(id.as_str());
                    }
                }
            }
            _ => continue,
        }
        last_expr.replace(expr);
    }

    for f_string in value.f_strings() {
        let mut has_name = false;
        for element in f_string.elements.expressions() {
            if let ast::Expr::Name(ast::ExprName { id, .. }) = element.expression.as_ref() {
                if arg_names.contains(id.as_str()) {
                    return false;
                }
                if semantic
                    .lookup_symbol(id)
                    .map_or(true, |id| semantic.binding(id).kind.is_builtin())
                {
                    return false;
                }
                has_name = true;
            }
            if let Some(spec) = &element.format_spec {
                let spec = &fstring_expr[spec.range()];
                if FormatSpec::parse(spec).is_err() {
                    return false;
                }
            }
        }
        if !has_name {
            return false;
        }
    }

    true
}

// fast check to disqualify any string literal without brackets
#[inline]
fn has_brackets(possible_fstring: &str) -> bool {
    // this qualifies rare false positives like "{ unclosed bracket"
    // but it's faster in the general case
    memchr2_iter(b'{', b'}', possible_fstring.as_bytes())
        .nth(1)
        .is_some()
}

fn fix_fstring_syntax(range: TextRange) -> Fix {
    Fix::unsafe_edit(Edit::insertion("f".into(), range.start()))
}
