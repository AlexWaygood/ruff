use std::fmt;

use aho_corasick::{AhoCorasick, AhoCorasickKind, Anchored, Input, MatchKind, StartKind};
use once_cell::sync::Lazy;

use ruff_text_size::{Ranged, TextLen, TextRange, TextSize};

/// Enumeration of the two kinds of quotes that can be used
/// for Python string/f-string/bytestring literals
#[derive(Debug, Default, Copy, Clone, Hash, PartialEq, Eq, is_macro::Is)]
pub enum Quote {
    /// E.g. `'`
    Single,
    /// E.g. `"`
    #[default]
    Double,
}

impl Quote {
    #[inline]
    pub const fn as_char(self) -> char {
        match self {
            Self::Single => '\'',
            Self::Double => '"',
        }
    }

    #[must_use]
    #[inline]
    pub const fn opposite(self) -> Self {
        match self {
            Self::Single => Self::Double,
            Self::Double => Self::Single,
        }
    }

    #[inline]
    pub const fn as_byte(self) -> u8 {
        match self {
            Self::Single => b'\'',
            Self::Double => b'"',
        }
    }
}

impl fmt::Display for Quote {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

impl TryFrom<char> for Quote {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '\'' => Ok(Quote::Single),
            '"' => Ok(Quote::Double),
            _ => Err(()),
        }
    }
}

/// Enumerations of the valid prefixes a string literal can have.
///
/// Bytestrings and f-strings are excluded from this enumeration,
/// as they are represented by different AST nodes.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, is_macro::Is)]
pub enum StringLiteralPrefix {
    /// Just a regular string with no prefixes
    Empty,

    /// A string with a `u` or `U` prefix, e.g. `u"foo"`.
    /// Note that, despite this variant's name,
    /// it is in fact a no-op at runtime to use the `u` or `U` prefix
    /// in Python. All Python-3 strings are unicode strings;
    /// this prefix is only allowed in Python 3 for backwards compatibility
    /// with Python 2. However, using this prefix in a Python string
    /// is mutually exclusive with an `r` or `R` prefix.
    Unicode,

    /// A "raw" string, that has an `r` or `R` prefix,
    /// e.g. `r"foo\."` or `R'bar\d'`.
    Raw { uppercase: bool },
}

impl StringLiteralPrefix {
    /// Return a `str` representation of the prefix
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Empty => "",
            Self::Unicode => "u",
            Self::Raw { uppercase: true } => "R",
            Self::Raw { uppercase: false } => "r",
        }
    }
}

impl fmt::Display for StringLiteralPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Enumeration of the valid prefixes an f-string literal can have.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FStringPrefix {
    /// Just a regular f-string with no other prefixes, e.g. f"{bar}"
    Regular,

    /// A "raw" format-string, that has an `r` or `R` prefix,
    /// e.g. `rf"{bar}"` or `Rf"{bar}"`
    Raw { uppercase_r: bool },
}

impl FStringPrefix {
    /// Return a `str` representation of the prefix
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Regular => "f",
            Self::Raw { uppercase_r: true } => "Rf",
            Self::Raw { uppercase_r: false } => "rf",
        }
    }

    /// Return true if this prefix indicates a "raw f-string",
    /// e.g. `rf"{bar}"` or `Rf"{bar}"`
    pub const fn is_raw(self) -> bool {
        matches!(self, Self::Raw { .. })
    }
}

impl fmt::Display for FStringPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Enumeration of the valid prefixes a bytestring literal can have.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ByteStringPrefix {
    /// Just a regular bytestring with no other prefixes, e.g. `b"foo"`
    Regular,

    /// A "raw" bytestring, that has an `r` or `R` prefix,
    /// e.g. `Rb"foo"` or `rb"foo"`
    Raw { uppercase_r: bool },
}

impl ByteStringPrefix {
    /// Return a `str` representation of the prefix
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Regular => "b",
            Self::Raw { uppercase_r: true } => "Rb",
            Self::Raw { uppercase_r: false } => "rb",
        }
    }

    /// Return true if this prefix indicates a "raw bytestring",
    /// e.g. `rb"foo"` or `Rb"foo"`
    pub const fn is_raw(self) -> bool {
        matches!(self, Self::Raw { .. })
    }
}

impl fmt::Display for ByteStringPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Enumeration of all the possible valid prefixes
/// prior to a Python string literal.
///
/// Using the `as_flags()` method on variants of this enum
/// is the recommended way to set `*_PREFIX` flags from the
/// `StringFlags` bitflag, as it means that you cannot accidentally
/// set a combination of `*_PREFIX` flags that would be invalid
/// at runtime in Python.
///
/// [String and Bytes literals]: https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals
/// [PEP 701]: https://peps.python.org/pep-0701/
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, is_macro::Is)]
pub enum StringPrefix {
    /// Prefixes that indicate the string is a bytestring
    Bytes(ByteStringPrefix),

    /// Prefixes that indicate the string is an f-string
    Format(FStringPrefix),

    /// All other prefixes
    Regular(StringLiteralPrefix),
}

impl TryFrom<char> for StringPrefix {
    type Error = String;

    fn try_from(value: char) -> Result<Self, String> {
        let result = match value {
            'r' => Self::Regular(StringLiteralPrefix::Raw { uppercase: false }),
            'R' => Self::Regular(StringLiteralPrefix::Raw { uppercase: true }),
            'u' | 'U' => Self::Regular(StringLiteralPrefix::Unicode),
            'b' | 'B' => Self::Bytes(ByteStringPrefix::Regular),
            'f' | 'F' => Self::Format(FStringPrefix::Regular),
            _ => return Err(format!("Unexpected prefix '{value}'")),
        };
        Ok(result)
    }
}

impl TryFrom<[char; 2]> for StringPrefix {
    type Error = String;

    fn try_from(value: [char; 2]) -> Result<Self, String> {
        let result = match value {
            ['r', 'f' | 'F'] | ['f' | 'F', 'r'] => {
                Self::Format(FStringPrefix::Raw { uppercase_r: false })
            }
            ['R', 'f' | 'F'] | ['f' | 'F', 'R'] => {
                Self::Format(FStringPrefix::Raw { uppercase_r: true })
            }
            ['r', 'b' | 'B'] | ['b' | 'B', 'r'] => {
                Self::Bytes(ByteStringPrefix::Raw { uppercase_r: false })
            }
            ['R', 'b' | 'B'] | ['b' | 'B', 'R'] => {
                Self::Bytes(ByteStringPrefix::Raw { uppercase_r: true })
            }
            _ => return Err(format!("Unexpected prefix '{}{}'", value[0], value[1])),
        };
        Ok(result)
    }
}

impl StringPrefix {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Regular(regular_prefix) => regular_prefix.as_str(),
            Self::Bytes(bytestring_prefix) => bytestring_prefix.as_str(),
            Self::Format(fstring_prefix) => fstring_prefix.as_str(),
        }
    }

    pub const fn is_raw(self) -> bool {
        matches!(
            self,
            Self::Regular(StringLiteralPrefix::Raw { .. })
                | Self::Bytes(ByteStringPrefix::Raw { .. })
                | Self::Format(FStringPrefix::Raw { .. })
        )
    }
}

impl fmt::Display for StringPrefix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Default for StringPrefix {
    fn default() -> Self {
        Self::Regular(StringLiteralPrefix::Empty)
    }
}

pub trait StringPart: Ranged + fmt::Debug {
    fn flags(&self) -> impl StringPartFlags;

    fn content_range(&self) -> TextRange {
        let flags = self.flags();
        TextRange::new(
            self.start() + flags.opener_len(),
            self.end() - flags.closer_len(),
        )
    }
}

pub trait StringPartFlags: Copy + fmt::Debug {
    /// Return the quoting style (single or double quotes)
    /// used by the string's opener and closer:
    /// - `"a"` -> `QuoteStyle::Double`
    /// - `'a'` -> `QuoteStyle::Single`
    fn quote_style(self) -> Quote;

    /// Return `true` if the string is triple-quoted, i.e.,
    /// it begins and ends with three consecutive quote characters.
    /// For example: `"""bar"""`
    fn is_triple_quoted(self) -> bool;

    fn prefix(self) -> StringPrefix;

    /// A `str` representation of the quotes used to start and close.
    /// This does not include any prefixes the string has in its opener.
    fn quote_str(self) -> &'static str {
        if self.is_triple_quoted() {
            match self.quote_style() {
                Quote::Single => "'''",
                Quote::Double => r#"""""#,
            }
        } else {
            match self.quote_style() {
                Quote::Single => "'",
                Quote::Double => "\"",
            }
        }
    }

    /// The length of the prefixes used (if any) in the string's opener.
    fn prefix_len(self) -> TextSize {
        self.prefix().as_str().text_len()
    }

    /// The length of the quotes used to start and close the string.
    /// This does not include the length of any prefixes the string has
    /// in its opener.
    fn quote_len(self) -> TextSize {
        if self.is_triple_quoted() {
            TextSize::new(3)
        } else {
            TextSize::new(1)
        }
    }

    /// The total length of the string's opener,
    /// i.e., the length of the prefixes plus the length
    /// of the quotes used to open the string.
    fn opener_len(self) -> TextSize {
        self.prefix_len() + self.quote_len()
    }

    /// The total length of the string's closer.
    /// This is always equal to `self.quote_len()`,
    /// but is provided here for symmetry with the `opener_len()` method.
    fn closer_len(self) -> TextSize {
        self.quote_len()
    }

    fn format_string_contents(self, contents: &str) -> String {
        format!(
            "{}{}{}{}",
            self.prefix(),
            self.quote_str(),
            contents,
            self.quote_str()
        )
    }
}

/// Includes all permutations of `r`, `u`, `f`, and `fr` (`ur` is invalid, as is `uf`). This
/// includes all possible orders, and all possible casings, for both single and triple quotes.
///
/// See: <https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals>
#[rustfmt::skip]
const TRIPLE_QUOTE_STR_PREFIXES: &[&str] = &[
    "FR\"\"\"",
    "Fr\"\"\"",
    "fR\"\"\"",
    "fr\"\"\"",
    "RF\"\"\"",
    "Rf\"\"\"",
    "rF\"\"\"",
    "rf\"\"\"",
    "FR'''",
    "Fr'''",
    "fR'''",
    "fr'''",
    "RF'''",
    "Rf'''",
    "rF'''",
    "rf'''",
    "R\"\"\"",
    "r\"\"\"",
    "R'''",
    "r'''",
    "F\"\"\"",
    "f\"\"\"",
    "F'''",
    "f'''",
    "U\"\"\"",
    "u\"\"\"",
    "U'''",
    "u'''",
    "\"\"\"",
    "'''",
];

#[rustfmt::skip]
const SINGLE_QUOTE_STR_PREFIXES: &[&str] = &[
    "FR\"",
    "Fr\"",
    "fR\"",
    "fr\"",
    "RF\"",
    "Rf\"",
    "rF\"",
    "rf\"",
    "FR'",
    "Fr'",
    "fR'",
    "fr'",
    "RF'",
    "Rf'",
    "rF'",
    "rf'",
    "R\"",
    "r\"",
    "R'",
    "r'",
    "F\"",
    "f\"",
    "F'",
    "f'",
    "U\"",
    "u\"",
    "U'",
    "u'",
    "\"",
    "'",
];

/// Includes all permutations of `b` and `rb`. This includes all possible orders, and all possible
/// casings, for both single and triple quotes.
///
/// See: <https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals>
#[rustfmt::skip]
pub const TRIPLE_QUOTE_BYTE_PREFIXES: &[&str] = &[
    "BR\"\"\"",
    "Br\"\"\"",
    "bR\"\"\"",
    "br\"\"\"",
    "RB\"\"\"",
    "Rb\"\"\"",
    "rB\"\"\"",
    "rb\"\"\"",
    "BR'''",
    "Br'''",
    "bR'''",
    "br'''",
    "RB'''",
    "Rb'''",
    "rB'''",
    "rb'''",
    "B\"\"\"",
    "b\"\"\"",
    "B'''",
    "b'''",
];

#[rustfmt::skip]
pub const SINGLE_QUOTE_BYTE_PREFIXES: &[&str] = &[
    "BR\"",
    "Br\"",
    "bR\"",
    "br\"",
    "RB\"",
    "Rb\"",
    "rB\"",
    "rb\"",
    "BR'",
    "Br'",
    "bR'",
    "br'",
    "RB'",
    "Rb'",
    "rB'",
    "rb'",
    "B\"",
    "b\"",
    "B'",
    "b'",
];

/// Strip the leading and trailing quotes from a string.
/// Assumes that the string is a valid string literal, but does not verify that the string
/// is a "simple" string literal (i.e., that it does not contain any implicit concatenations).
pub fn raw_contents(contents: &str) -> Option<&str> {
    let range = raw_contents_range(contents)?;

    Some(&contents[range])
}

pub fn raw_contents_range(contents: &str) -> Option<TextRange> {
    let leading_quote_str = leading_quote(contents)?;
    let trailing_quote_str = trailing_quote(contents)?;

    Some(TextRange::new(
        leading_quote_str.text_len(),
        contents.text_len() - trailing_quote_str.text_len(),
    ))
}

/// An [`AhoCorasick`] matcher for string and byte literal prefixes.
static PREFIX_MATCHER: Lazy<AhoCorasick> = Lazy::new(|| {
    AhoCorasick::builder()
        .start_kind(StartKind::Anchored)
        .match_kind(MatchKind::LeftmostLongest)
        .kind(Some(AhoCorasickKind::DFA))
        .build(
            TRIPLE_QUOTE_STR_PREFIXES
                .iter()
                .chain(TRIPLE_QUOTE_BYTE_PREFIXES)
                .chain(SINGLE_QUOTE_STR_PREFIXES)
                .chain(SINGLE_QUOTE_BYTE_PREFIXES),
        )
        .unwrap()
});

/// Return the leading quote for a string or byte literal (e.g., `"""`).
pub fn leading_quote(content: &str) -> Option<&str> {
    let mat = PREFIX_MATCHER.find(Input::new(content).anchored(Anchored::Yes))?;
    Some(&content[mat.start()..mat.end()])
}

/// Return the trailing quote string for a string or byte literal (e.g., `"""`).
pub fn trailing_quote(content: &str) -> Option<&str> {
    if content.ends_with("'''") {
        Some("'''")
    } else if content.ends_with("\"\"\"") {
        Some("\"\"\"")
    } else if content.ends_with('\'') {
        Some("'")
    } else if content.ends_with('\"') {
        Some("\"")
    } else {
        None
    }
}

/// Return `true` if the string is a triple-quote string or byte prefix.
pub fn is_triple_quote(content: &str) -> bool {
    TRIPLE_QUOTE_STR_PREFIXES.contains(&content) || TRIPLE_QUOTE_BYTE_PREFIXES.contains(&content)
}

#[cfg(test)]
mod tests {
    use super::{
        SINGLE_QUOTE_BYTE_PREFIXES, SINGLE_QUOTE_STR_PREFIXES, TRIPLE_QUOTE_BYTE_PREFIXES,
        TRIPLE_QUOTE_STR_PREFIXES,
    };

    #[test]
    fn prefix_uniqueness() {
        let prefixes = TRIPLE_QUOTE_STR_PREFIXES
            .iter()
            .chain(TRIPLE_QUOTE_BYTE_PREFIXES)
            .chain(SINGLE_QUOTE_STR_PREFIXES)
            .chain(SINGLE_QUOTE_BYTE_PREFIXES)
            .collect::<Vec<_>>();
        for (i, prefix_i) in prefixes.iter().enumerate() {
            for (j, prefix_j) in prefixes.iter().enumerate() {
                if i > j {
                    assert!(
                        !prefix_i.starts_with(*prefix_j),
                        "Prefixes are not unique: {prefix_i} starts with {prefix_j}",
                    );
                }
            }
        }
    }
}
