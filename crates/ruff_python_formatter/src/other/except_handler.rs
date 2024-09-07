use ruff_formatter::write;
use ruff_formatter::FormatRuleWithOptions;
use ruff_python_ast::ExceptHandler;

use crate::expression::maybe_parenthesize_expression;
use crate::expression::parentheses::Parenthesize;
use crate::prelude::*;
use crate::statement::clause::{clause_body, clause_header, ClauseHeader};
use crate::statement::suite::SuiteKind;

#[derive(Copy, Clone, Default)]
pub(crate) enum ExceptHandlerKind {
    #[default]
    Regular,
    Starred,
}

#[derive(Default)]
pub struct FormatExceptHandler {
    pub(crate) except_handler_kind: ExceptHandlerKind,
    pub(crate) last_suite_in_statement: bool,
}

impl FormatRuleWithOptions<ExceptHandler, PyFormatContext<'_>> for FormatExceptHandler {
    type Options = FormatExceptHandler;

    fn with_options(mut self, options: Self::Options) -> Self {
        self.except_handler_kind = options.except_handler_kind;
        self.last_suite_in_statement = options.last_suite_in_statement;
        self
    }
}

impl FormatNodeRule<ExceptHandler> for FormatExceptHandler {
    fn fmt_fields(&self, item: &ExceptHandler, f: &mut PyFormatter) -> FormatResult<()> {
        let except_handler_kind = self.except_handler_kind;
        let ExceptHandler {
            range: _,
            type_,
            name,
            body,
        } = item;

        let comments_info = f.context().comments().clone();
        let dangling_comments = comments_info.dangling(item);

        write!(
            f,
            [
                clause_header(
                    ClauseHeader::ExceptHandler(item),
                    dangling_comments,
                    &format_with(|f| {
                        write!(
                            f,
                            [
                                token("except"),
                                match except_handler_kind {
                                    ExceptHandlerKind::Regular => None,
                                    ExceptHandlerKind::Starred => Some(token("*")),
                                }
                            ]
                        )?;

                        if let Some(type_) = type_ {
                            write!(
                                f,
                                [
                                    space(),
                                    maybe_parenthesize_expression(
                                        type_,
                                        item,
                                        Parenthesize::IfBreaks
                                    )
                                ]
                            )?;
                            if let Some(name) = name {
                                write!(f, [space(), token("as"), space(), name.format()])?;
                            }
                        }

                        Ok(())
                    }),
                ),
                clause_body(
                    body,
                    SuiteKind::other(self.last_suite_in_statement),
                    dangling_comments
                ),
            ]
        )
    }
}
