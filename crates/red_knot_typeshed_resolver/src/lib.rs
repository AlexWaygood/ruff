use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use versions::{TypeshedVersions, TypeshedVersionsParseError};

mod typeshed_vendored;
pub mod versions;

pub const TYPESHED_STDLIB_DIR: &str = "stdlib";
const STDLIB_VERSIONS_FILE: &str = "VERSIONS";

const CORE_TYPING_MODULES: &[&str] = &[
    "_collections_abc.pyi",
    "builtins.pyi",
    "collections/abc.pyi",
    "types.pyi",
    "typing.pyi",
    "typing_extensions.pyi",
];

#[derive(Debug, Default)]
pub enum Typeshed {
    #[default]
    Vendored,

    Custom(CustomTypeshed),
}

impl Typeshed {
    pub fn from_custom_typeshed_dir(dir: &Path) -> Result<Self, InvalidTypeshedError> {
        Ok(Self::Custom(CustomTypeshed::from_custom_typeshed_dir(dir)?))
    }
}

#[derive(Debug)]
pub struct CustomTypeshed {
    stdlib_path: PathBuf,
    versions: TypeshedVersions,
}

impl CustomTypeshed {
    fn from_custom_typeshed_dir(dir: &Path) -> Result<Self, InvalidTypeshedError> {
        let stdlib_path = dir.join(TYPESHED_STDLIB_DIR);
        if !stdlib_path.exists() {
            return Err(InvalidTypeshedError::NoStdlibDirectory);
        }
        for module in CORE_TYPING_MODULES {
            if !stdlib_path.join(module).exists() {
                return Err(InvalidTypeshedError::CoreModuleMissing(module));
            }
        }
        let raw_versions = fs::read_to_string(stdlib_path.join(STDLIB_VERSIONS_FILE))?;
        Ok(Self {
            stdlib_path,
            versions: raw_versions.parse()?,
        })
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InvalidTypeshedError {
    #[error("Provided directory does not contain a `stdlib/` subdirectory")]
    NoStdlibDirectory,
    #[error("`stdlib/` directory is missing core module {0}")]
    CoreModuleMissing(&'static str),
    #[error("Could not read `stdlib/VERSIONS` file from the provided directory")]
    NoVersionsFile(#[from] io::Error),
    #[error("{0}")]
    VersionsParseError(#[from] TypeshedVersionsParseError),
}
