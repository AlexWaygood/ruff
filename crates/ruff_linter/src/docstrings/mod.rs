use std::fmt::{Debug, Formatter};
use std::ops::Deref;

use ruff_python_ast::{AnyStringFlags, StringFlags, StringLiteral, StringPart};
use ruff_python_semantic::Definition;
use ruff_source_file::LineRanges;
use ruff_text_size::{Ranged, TextRange};

pub(crate) mod extraction;
pub(crate) mod google;
pub(crate) mod numpy;
pub(crate) mod sections;
pub(crate) mod styles;

#[derive(Debug)]
pub(crate) struct Docstring<'a> {
    pub(crate) definition: &'a Definition<'a>,
    /// The literal AST node representing the docstring.
    pub(crate) expr: &'a StringLiteral,
    /// The source file the docstring was defined in.
    pub(crate) source: &'a str,
}

impl<'a> Docstring<'a> {
    /// The contents of the docstring, including the opening and closing quotes.
    pub(crate) fn contents(&self) -> &'a str {
        &self.source[self.range()]
    }

    /// The contents of the docstring, excluding the opening and closing quotes.
    pub(crate) fn body(&self) -> DocstringBody {
        DocstringBody { docstring: self }
    }

    pub(crate) fn indentation(&self) -> &'a str {
        &self.source[TextRange::new(self.source.line_start(self.start()), self.start())]
    }

    /// The docstring's "opener" (the string's prefix, if any, and its opening quotes).
    pub(crate) fn opener(&self) -> &'a str {
        &self.source[TextRange::new(self.start(), self.start() + self.flags().opener_len())]
    }

    /// The docstring's closing quotes.
    pub(crate) fn closer(&self) -> &'a str {
        &self.source[TextRange::new(self.end() - self.flags().closer_len(), self.end())]
    }
}

impl Ranged for Docstring<'_> {
    fn range(&self) -> TextRange {
        self.expr.range()
    }
}

impl StringPart for Docstring<'_> {
    fn flags(&self) -> AnyStringFlags {
        self.expr.flags()
    }
}

#[derive(Copy, Clone)]
pub(crate) struct DocstringBody<'a> {
    docstring: &'a Docstring<'a>,
}

impl<'a> DocstringBody<'a> {
    pub(crate) fn as_str(self) -> &'a str {
        &self.docstring.source[self.docstring.raw_contents_range()]
    }
}

impl Ranged for DocstringBody<'_> {
    fn range(&self) -> TextRange {
        self.docstring.raw_contents_range()
    }
}

impl Deref for DocstringBody<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Debug for DocstringBody<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DocstringBody")
            .field("text", &self.as_str())
            .field("range", &self.range())
            .finish()
    }
}
