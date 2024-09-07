use ruff_formatter::write;
use ruff_python_ast::StmtTry;
use ruff_text_size::Ranged;

use crate::comments;
use crate::comments::leading_alternate_branch_comments;
use crate::comments::SourceComment;
use crate::other::except_handler::{ExceptHandlerKind, FormatExceptHandler};
use crate::prelude::*;
use crate::statement::clause::{clause_body, clause_header, ClauseHeader, ElseClause};
use crate::statement::suite::SuiteKind;
use crate::statement::Stmt;

#[derive(Default)]
pub struct FormatStmtTry;

impl FormatNodeRule<StmtTry> for FormatStmtTry {
    fn fmt_fields(&self, item: &StmtTry, f: &mut PyFormatter) -> FormatResult<()> {
        let StmtTry {
            body,
            handlers,
            orelse,
            finalbody,
            is_star,
            range: _,
        } = item;

        let comments_info = f.context().comments().clone();
        let mut dangling_comments = comments_info.dangling(item);

        (_, dangling_comments) =
            format_case(item, CaseKind::Try, None, dangling_comments, false, f)?;
        let mut previous_node = body.last();

        for except_handler in handlers {
            let handler_comments = comments_info.leading(except_handler);
            let except_handler_kind = if *is_star {
                ExceptHandlerKind::Starred
            } else {
                ExceptHandlerKind::Regular
            };
            let last_suite_in_statement = except_handler == handlers.last().unwrap()
                && orelse.is_empty()
                && finalbody.is_empty();

            write!(
                f,
                [
                    leading_alternate_branch_comments(handler_comments, previous_node),
                    &except_handler.format().with_options(FormatExceptHandler {
                        except_handler_kind,
                        last_suite_in_statement
                    })
                ]
            )?;
            previous_node = except_handler.body.last();
        }

        (previous_node, dangling_comments) = format_case(
            item,
            CaseKind::Else,
            previous_node,
            dangling_comments,
            finalbody.is_empty(),
            f,
        )?;

        format_case(
            item,
            CaseKind::Finally,
            previous_node,
            dangling_comments,
            true,
            f,
        )?;

        write!(f, [comments::dangling_comments(dangling_comments)])
    }
}

fn format_case<'a>(
    try_statement: &'a StmtTry,
    kind: CaseKind,
    previous_node: Option<&'a Stmt>,
    dangling_comments: &'a [SourceComment],
    last_suite_in_statement: bool,
    f: &mut PyFormatter,
) -> FormatResult<(Option<&'a Stmt>, &'a [SourceComment])> {
    let body = match kind {
        CaseKind::Try => &try_statement.body,
        CaseKind::Else => &try_statement.orelse,
        CaseKind::Finally => &try_statement.finalbody,
    };

    Ok(if let Some(last) = body.last() {
        let case_comments_start =
            dangling_comments.partition_point(|comment| comment.end() <= last.end());
        let (case_comments, rest) = dangling_comments.split_at(case_comments_start);
        let partition_point =
            case_comments.partition_point(|comment| comment.line_position().is_own_line());

        let (leading_case_comments, trailing_case_comments) =
            case_comments.split_at(partition_point);

        let header = match kind {
            CaseKind::Try => ClauseHeader::Try(try_statement),
            CaseKind::Else => ClauseHeader::OrElse(ElseClause::Try(try_statement)),
            CaseKind::Finally => ClauseHeader::TryFinally(try_statement),
        };

        write!(
            f,
            [
                clause_header(header, trailing_case_comments, &token(kind.keyword()))
                    .with_leading_comments(leading_case_comments, previous_node),
                clause_body(
                    body,
                    SuiteKind::other(last_suite_in_statement),
                    trailing_case_comments
                ),
            ]
        )?;
        (Some(last), rest)
    } else {
        (previous_node, dangling_comments)
    })
}

#[derive(Copy, Clone)]
enum CaseKind {
    Try,
    Else,
    Finally,
}

impl CaseKind {
    fn keyword(self) -> &'static str {
        match self {
            CaseKind::Try => "try",
            CaseKind::Else => "else",
            CaseKind::Finally => "finally",
        }
    }
}
