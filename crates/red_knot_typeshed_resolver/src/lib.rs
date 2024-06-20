use std::fs;
use std::io;
use std::path::{Path, PathBuf};

pub use py_version::SupportedPyVersion;
use ruff_db::file_system::OsFileSystem;
use ruff_db::module_name::ModuleName;
use ruff_db::vfs::Vfs;
use versions::{TypeshedVersions, TypeshedVersionsParseError};

mod constants;
mod py_version;
mod typeshed_vendored;
mod versions;

#[derive(Debug, Default)]
pub struct TypeshedResolver {
    typeshed: Typeshed,
    target_version: SupportedPyVersion,
}

impl TypeshedResolver {
    fn versions(&self) -> &TypeshedVersions {
        match &self.typeshed {
            Typeshed::Custom(CustomTypeshed {versions, .. }) => versions,
            Typeshed::Vendored => &typeshed_vendored::VENDORED_TYPESHED_VERSIONS,
        }
    }

    fn exists(&self, module: ModuleName, vfs: &Vfs) -> bool {
        let target_version = self.target_version.into();
        let versions = self.versions();
        if let Some(range) = versions.get(&module) {
            return range.contains(target_version);
        }
        let mut module = Some(module);
        while let Some(module_to_try) = module {
            if let Some(range) = versions.get(&module_to_try) {
                if range.contains(target_version) {
                    break;
                }
                return false;
            }
            module = module_to_try.parent();
        }
        false
    }
}

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
    /// Validate a directory passed by the user
    /// that is to be used as the single source of truth for the stdlib.
    ///
    /// Only basic validation is done here;
    /// a certain amount of trust is placed in the user to provide something sane.
    /// The same invariants (and more) are validated in `typeshed_vendored.rs`
    /// for the vendored typeshed stubs bundled with the Ruff binary.
    fn from_custom_typeshed_dir(dir: &Path) -> Result<Self, InvalidTypeshedError> {
        let stdlib_path = dir.join(constants::TYPESHED_STDLIB_DIR);
        if !stdlib_path.exists() {
            return Err(InvalidTypeshedError::NoStdlibDirectory);
        }
        for module in constants::CORE_TYPING_MODULES {
            if !stdlib_path.join(module).exists() {
                return Err(InvalidTypeshedError::CoreModuleMissing(module));
            }
        }
        let raw_versions = fs::read_to_string(stdlib_path.join(constants::STDLIB_VERSIONS_FILE))?;
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
