use std::fmt;
use std::num::{NonZeroUsize, NonZeroU16};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct TypeshedVersionsParseError {
    pub line_number: Option<NonZeroU16>,
    pub reason: TypeshedVersionsParseErrorKind,
}

impl fmt::Display for TypeshedVersionsParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TypeshedVersionsParseError {
            line_number,
            reason,
        } = self;
        if let Some(line_number) = line_number {
            write!(
                f,
                "Error while parsing line {line_number} of typeshed's VERSIONS file: {reason}"
            )
        } else {
            write!(f, "Error while parsing typeshed's VERSIONS file: {reason}")
        }
    }
}

impl std::error::Error for TypeshedVersionsParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let TypeshedVersionsParseErrorKind::IntegerParsingFailure { err, .. } = &self.reason {
            Some(err)
        } else {
            None
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TypeshedVersionsParseErrorKind {
    TooManyLines(NonZeroUsize),
    UnexpectedNumberOfColons,
    InvalidModuleName(String),
    UnexpectedNumberOfHyphens,
    UnexpectedNumberOfPeriods(String),
    IntegerParsingFailure {
        version: String,
        err: std::num::ParseIntError,
    },
}

impl fmt::Display for TypeshedVersionsParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooManyLines(num_lines) => write!(
                f,
                "File has too many lines ({num_lines}); maximum allowed is {}",
                NonZeroU16::MAX
            ),
            Self::UnexpectedNumberOfColons => {
                f.write_str("Expected every non-comment line to have exactly one colon")
            }
            Self::InvalidModuleName(name) => write!(
                f,
                "Expected all components of '{name}' to be valid Python identifiers"
            ),
            Self::UnexpectedNumberOfHyphens => {
                f.write_str("Expected every non-comment line to have exactly one '-' character")
            }
            Self::UnexpectedNumberOfPeriods(format) => write!(
                f,
                "Expected all versions to be in the form {{MAJOR}}.{{MINOR}}; got '{format}'"
            ),
            Self::IntegerParsingFailure { version, err } => write!(
                f,
                "Failed to convert '{version}' to a pair of integers due to {err}",
            ),
        }
    }
}
