use std::fmt::{Debug, Formatter};
use std::ops::Deref;

use ruff_python_ast::{AnyStringFlags, StringFlags, StringLiteral, StringPart};
use ruff_python_semantic::Definition;
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
    /// The content of the docstring, including the leading and trailing quotes.
    pub(crate) contents: &'a str,
    pub(crate) indentation: &'a str,
}

impl<'a> Docstring<'a> {
    pub(crate) fn body(&self) -> DocstringBody {
        DocstringBody { docstring: self }
    }

    /// The range of the docstring body (without the quotes). The range is relative to [`Self::contents`].
    pub(crate) fn body_range(&self) -> TextRange {
        self.raw_contents_range() - self.start()
    }

    /// The docstring's "opener" (the string's prefix, if any, and its opening quotes).
    pub(crate) fn opener(&self) -> &'a str {
        &self.contents[TextRange::up_to(self.body_range().start())]
    }

    /// The docstring's closing quotes.
    pub(crate) fn closer(&self) -> &'a str {
        let contents_end = self.body_range().end();
        &self.contents[TextRange::new(contents_end, contents_end + self.flags().closer_len())]
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
        &self.docstring.contents[self.docstring.body_range()]
    }
}

impl Ranged for DocstringBody<'_> {
    fn range(&self) -> TextRange {
        self.docstring.body_range() + self.docstring.start()
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
