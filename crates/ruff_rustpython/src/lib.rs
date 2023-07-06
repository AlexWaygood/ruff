use rustpython_parser as parser;
use rustpython_parser::ast::{Mod, ModModule, Suite};
use rustpython_parser::lexer::LexResult;
use rustpython_parser::{lexer, Mode, ParseError};

/// Collect tokens up to and including the first error.
pub fn tokenize(contents: &str, mode: Mode) -> Vec<LexResult> {
    let mut tokens: Vec<LexResult> = vec![];
    for tok in lexer::lex(contents, mode) {
        let is_err = tok.is_err();
        tokens.push(tok);
        if is_err {
            break;
        }
    }
    tokens
}

/// Parse a full Python program from its tokens.
pub fn parse_program_tokens(
    lxr: Vec<LexResult>,
    mode: Mode,
    source_path: &str,
) -> anyhow::Result<Suite, ParseError> {
    parser::parse_tokens(lxr, mode, source_path).map(|top| match top {
        Mod::Module(ModModule { body, .. }) => body,
        _ => unreachable!(),
    })
}
