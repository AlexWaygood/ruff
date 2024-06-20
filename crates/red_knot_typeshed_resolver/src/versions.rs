use std::collections::BTreeMap;
use std::fmt;
use std::num::{NonZeroU16, NonZeroUsize};
use std::ops::{RangeFrom, RangeInclusive};
use std::str::FromStr;

use rustc_hash::FxHashMap;

use ruff_db::module_name::ModuleName;

use crate::py_version::SupportedPyVersion;

pub fn module_exists_on_version(
    data: &TypeshedVersions,
    module: &ModuleName,
    version: impl Into<PyVersion>,
) -> VersionQueryResult {
    let version = version.into();
    if let Some(range) = data.get(module) {
        if range.contains(version) {
            VersionQueryResult::Yes
        } else {
            VersionQueryResult::No
        }
    } else {
        let mut module = module.parent();
        while let Some(module_to_try) = module {
            if let Some(range) = data.get(&module_to_try) {
                if range.contains(version) {
                    return VersionQueryResult::Maybe;
                }
            }
            module = module_to_try.parent();
        }
        VersionQueryResult::No
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VersionQueryResult {
    /// The module definitely exists on the specified Python version
    Yes,

    /// The module definitely does not exist on the specified Python version
    No,

    /// The module might exist on the specified Python version, or it might not
    /// (you'll have to check the vendored zip to check!)
    Maybe,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeshedVersionsParseError {
    line_number: NonZeroU16,
    reason: TypeshedVersionsParseErrorKind,
}

impl fmt::Display for TypeshedVersionsParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TypeshedVersionsParseError {
            line_number,
            reason,
        } = self;
        write!(
            f,
            "Error while parsing line {line_number} of typeshed's VERSIONS file: {reason}"
        )
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

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum TypeshedVersionsParseErrorKind {
    #[error(
        "File has too many lines ({0}); maximum allowed is {}",
        NonZeroU16::MAX
    )]
    TooManyLines(NonZeroUsize),
    #[error("Expected every non-comment line to have exactly one colon")]
    UnexpectedNumberOfColons,
    #[error("Expected all components of '{0}' to be valid Python identifiers")]
    InvalidModuleName(String),
    #[error("Expected every non-comment line to have exactly one '-' character")]
    UnexpectedNumberOfHyphens,
    #[error("Expected all versions to be in the form {{MAJOR}}.{{MINOR}}; got '{0}'")]
    UnexpectedNumberOfPeriods(String),
    #[error("Failed to convert '{version}' to a pair of integers due to {err}")]
    IntegerParsingFailure {
        version: String,
        err: std::num::ParseIntError,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeshedVersions(FxHashMap<ModuleName, PyVersionRange>);

impl TypeshedVersions {
    pub(crate) fn get(&self, module: &ModuleName) -> Option<&PyVersionRange> {
        self.0.get(module)
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn contains_module(&self, module_name: &ModuleName) -> bool {
        self.0.contains_key(module_name)
    }

    pub fn module_exists_on_version(
        &self,
        module: ModuleName,
        version: impl Into<PyVersion>,
    ) -> bool {
        let version = version.into();
        let mut module: Option<ModuleName> = Some(module);
        while let Some(module_to_try) = module {
            if let Some(range) = self.0.get(&module_to_try) {
                return range.contains(version);
            }
            module = module_to_try.parent();
        }
        false
    }
}

impl FromStr for TypeshedVersions {
    type Err = TypeshedVersionsParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut map = FxHashMap::default();

        for (line_index, line) in s.lines().enumerate() {
            // humans expect line numbers to be 1-indexed
            let line_number = NonZeroUsize::new(line_index.saturating_add(1)).unwrap();

            let Ok(line_number) = NonZeroU16::try_from(line_number) else {
                return Err(TypeshedVersionsParseError {
                    line_number: NonZeroU16::MAX,
                    reason: TypeshedVersionsParseErrorKind::TooManyLines(line_number),
                });
            };

            let Some(content) = line.split('#').map(str::trim).next() else {
                continue;
            };
            if content.is_empty() {
                continue;
            }

            let mut parts = content.split(':').map(str::trim);
            let (Some(module_name), Some(rest), None) = (parts.next(), parts.next(), parts.next())
            else {
                return Err(TypeshedVersionsParseError {
                    line_number,
                    reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfColons,
                });
            };

            let Some(module_name) = ModuleName::new(module_name) else {
                return Err(TypeshedVersionsParseError {
                    line_number,
                    reason: TypeshedVersionsParseErrorKind::InvalidModuleName(
                        module_name.to_string(),
                    ),
                });
            };

            match PyVersionRange::from_str(rest) {
                Ok(version) => map.insert(module_name, version),
                Err(reason) => {
                    return Err(TypeshedVersionsParseError {
                        line_number,
                        reason,
                    })
                }
            };
        }

        Ok(Self(map))
    }
}

impl fmt::Display for TypeshedVersions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sorted_items: BTreeMap<&ModuleName, &PyVersionRange> = self.0.iter().collect();
        for (module_name, range) in sorted_items {
            writeln!(f, "{module_name}: {range}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) enum PyVersionRange {
    AvailableFrom(RangeFrom<PyVersion>),
    AvailableWithin(RangeInclusive<PyVersion>),
}

impl PyVersionRange {
    pub(crate) fn contains(&self, version: PyVersion) -> bool {
        match self {
            Self::AvailableFrom(inner) => inner.contains(&version),
            Self::AvailableWithin(inner) => inner.contains(&version),
        }
    }
}

impl FromStr for PyVersionRange {
    type Err = TypeshedVersionsParseErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('-').map(str::trim);
        match (parts.next(), parts.next(), parts.next()) {
            (Some(lower), Some(""), None) => Ok(Self::AvailableFrom((lower.parse()?)..)),
            (Some(lower), Some(upper), None) => {
                Ok(Self::AvailableWithin((lower.parse()?)..=(upper.parse()?)))
            }
            _ => Err(TypeshedVersionsParseErrorKind::UnexpectedNumberOfHyphens),
        }
    }
}

impl fmt::Display for PyVersionRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AvailableFrom(range_from) => write!(f, "{}-", range_from.start),
            Self::AvailableWithin(range_inclusive) => {
                write!(f, "{}-{}", range_inclusive.start(), range_inclusive.end())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct PyVersion {
    major: u8,
    minor: u8,
}

impl FromStr for PyVersion {
    type Err = TypeshedVersionsParseErrorKind;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('.').map(str::trim);
        let (Some(major), Some(minor), None) = (parts.next(), parts.next(), parts.next()) else {
            return Err(TypeshedVersionsParseErrorKind::UnexpectedNumberOfPeriods(
                s.to_string(),
            ));
        };
        let major = match u8::from_str(major) {
            Ok(major) => major,
            Err(err) => {
                return Err(TypeshedVersionsParseErrorKind::IntegerParsingFailure {
                    version: s.to_string(),
                    err,
                })
            }
        };
        let minor = match u8::from_str(minor) {
            Ok(minor) => minor,
            Err(err) => {
                return Err(TypeshedVersionsParseErrorKind::IntegerParsingFailure {
                    version: s.to_string(),
                    err,
                })
            }
        };
        Ok(Self { major, minor })
    }
}

impl fmt::Display for PyVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let PyVersion { major, minor } = self;
        write!(f, "{major}.{minor}")
    }
}

impl From<SupportedPyVersion> for PyVersion {
    fn from(value: SupportedPyVersion) -> Self {
        match value {
            SupportedPyVersion::Py37 => PyVersion { major: 3, minor: 7 },
            SupportedPyVersion::Py38 => PyVersion { major: 3, minor: 8 },
            SupportedPyVersion::Py39 => PyVersion { major: 3, minor: 9 },
            SupportedPyVersion::Py310 => PyVersion {
                major: 3,
                minor: 10,
            },
            SupportedPyVersion::Py311 => PyVersion {
                major: 3,
                minor: 11,
            },
            SupportedPyVersion::Py312 => PyVersion {
                major: 3,
                minor: 12,
            },
            SupportedPyVersion::Py313 => PyVersion {
                major: 3,
                minor: 13,
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::num::{IntErrorKind, NonZeroU16};

    use super::*;

    use insta::assert_snapshot;

    #[allow(unsafe_code)]
    const ONE: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(1) };

    #[test]
    fn can_parse_vendored_versions_file() {
        let versions_data = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/vendor/typeshed/stdlib/VERSIONS"
        ));

        let versions = TypeshedVersions::from_str(versions_data).unwrap();
        assert!(versions.len() > 100);
        assert!(versions.len() < 1000);

        let asyncio = ModuleName::new_static("asyncio").unwrap();
        let asyncio_staggered = ModuleName::new_static("asyncio.staggered").unwrap();
        let audioop = ModuleName::new_static("audioop").unwrap();

        assert!(versions.contains_module(&asyncio));
        assert!(versions.module_exists_on_version(asyncio, SupportedPyVersion::Py310));

        assert!(versions.contains_module(&asyncio_staggered));
        assert!(
            versions.module_exists_on_version(asyncio_staggered.clone(), SupportedPyVersion::Py38)
        );
        assert!(!versions.module_exists_on_version(asyncio_staggered, SupportedPyVersion::Py37));

        assert!(versions.contains_module(&audioop));
        assert!(versions.module_exists_on_version(audioop.clone(), SupportedPyVersion::Py312));
        assert!(!versions.module_exists_on_version(audioop, SupportedPyVersion::Py313));
    }

    #[test]
    fn can_parse_mock_versions_file() {
        const VERSIONS: &str = "\
# a comment
    # some more comment
# yet more comment


# and some more comment

bar: 2.7-3.10

# more comment
bar.baz: 3.1-3.9
foo: 3.8-   # trailing comment
";
        let parsed_versions = TypeshedVersions::from_str(VERSIONS).unwrap();
        assert_eq!(parsed_versions.len(), 3);
        assert_snapshot!(parsed_versions.to_string(), @r###"
        bar: 2.7-3.10
        bar.baz: 3.1-3.9
        foo: 3.8-
        "###
        );

        let foo = ModuleName::new_static("foo").unwrap();
        let bar = ModuleName::new_static("bar").unwrap();
        let bar_baz = ModuleName::new_static("bar.baz").unwrap();
        let spam = ModuleName::new_static("spam").unwrap();

        assert!(parsed_versions.contains_module(&foo));
        assert!(!parsed_versions.module_exists_on_version(foo.clone(), SupportedPyVersion::Py37));
        assert!(parsed_versions.module_exists_on_version(foo.clone(), SupportedPyVersion::Py38));
        assert!(parsed_versions.module_exists_on_version(foo, SupportedPyVersion::Py311));

        assert!(parsed_versions.contains_module(&bar));
        assert!(parsed_versions.module_exists_on_version(bar.clone(), SupportedPyVersion::Py37));
        assert!(parsed_versions.module_exists_on_version(bar.clone(), SupportedPyVersion::Py310));
        assert!(!parsed_versions.module_exists_on_version(bar, SupportedPyVersion::Py311));

        assert!(parsed_versions.contains_module(&bar_baz));
        assert!(parsed_versions.module_exists_on_version(bar_baz.clone(), SupportedPyVersion::Py37));
        assert!(parsed_versions.module_exists_on_version(bar_baz.clone(), SupportedPyVersion::Py39));
        assert!(!parsed_versions.module_exists_on_version(bar_baz, SupportedPyVersion::Py310));

        assert!(!parsed_versions.contains_module(&spam));
        assert!(!parsed_versions.module_exists_on_version(spam.clone(), SupportedPyVersion::Py37));
        assert!(!parsed_versions.module_exists_on_version(spam, SupportedPyVersion::Py313));
    }

    #[test]
    fn invalid_huge_versions_file() {
        let offset = 100;
        let too_many = u16::MAX as usize + offset;

        let mut massive_versions_file = String::new();
        for i in 0..too_many {
            massive_versions_file.push_str(&format!("x{i}: 3.8-\n"));
        }

        assert_eq!(
            TypeshedVersions::from_str(&massive_versions_file),
            Err(TypeshedVersionsParseError {
                line_number: NonZeroU16::MAX,
                reason: TypeshedVersionsParseErrorKind::TooManyLines(
                    NonZeroUsize::new(too_many + 1 - offset).unwrap()
                )
            })
        );
    }

    #[test]
    fn invalid_typeshed_versions_bad_colon_number() {
        assert_eq!(
            TypeshedVersions::from_str("foo 3.7"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfColons
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("foo:: 3.7"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfColons
            })
        );
    }

    #[test]
    fn invalid_typeshed_versions_non_identifier_modules() {
        assert_eq!(
            TypeshedVersions::from_str("not!an!identifier!: 3.7"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::InvalidModuleName(
                    "not!an!identifier!".to_string()
                )
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("(also_not).(an_identifier): 3.7"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::InvalidModuleName(
                    "(also_not).(an_identifier)".to_string()
                )
            })
        );
    }

    #[test]
    fn invalid_typeshed_versions_bad_hyphen_number() {
        assert_eq!(
            TypeshedVersions::from_str("foo: 3.8"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfHyphens
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("foo: 3.8--"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfHyphens
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("foo: 3.8--3.9"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfHyphens
            })
        );
    }

    #[test]
    fn invalid_typeshed_versions_bad_period_number() {
        assert_eq!(
            TypeshedVersions::from_str("foo: 38-"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfPeriods("38".to_string())
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("foo: 3..8-"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfPeriods(
                    "3..8".to_string()
                )
            })
        );
        assert_eq!(
            TypeshedVersions::from_str("foo: 3.8-3..11"),
            Err(TypeshedVersionsParseError {
                line_number: ONE,
                reason: TypeshedVersionsParseErrorKind::UnexpectedNumberOfPeriods(
                    "3..11".to_string()
                )
            })
        );
    }

    #[test]
    fn invalid_typeshed_versions_non_digits() {
        let err = TypeshedVersions::from_str("foo: 1.two-").unwrap_err();
        assert_eq!(err.line_number, ONE);
        let TypeshedVersionsParseErrorKind::IntegerParsingFailure { version, err } = err.reason
        else {
            panic!()
        };
        assert_eq!(version, "1.two".to_string());
        assert_eq!(*err.kind(), IntErrorKind::InvalidDigit);

        let err = TypeshedVersions::from_str("foo: 3.8-four.9").unwrap_err();
        assert_eq!(err.line_number, ONE);
        let TypeshedVersionsParseErrorKind::IntegerParsingFailure { version, err } = err.reason
        else {
            panic!()
        };
        assert_eq!(version, "four.9".to_string());
        assert_eq!(*err.kind(), IntErrorKind::InvalidDigit);
    }
}
