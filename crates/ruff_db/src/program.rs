use crate::files::File;
use crate::search_path_settings::{
    try_resolve_module_resolution_settings, SearchPathSettings, SearchPathValidationError,
    StaticSearchPaths,
};
use crate::typeshed_version_error::TypeshedVersionsParseError;
use crate::Db;
use salsa::Durability;

#[salsa::input(singleton)]
pub struct Program {
    pub target_version: TargetVersion,

    #[return_ref]
    pub static_search_paths: StaticSearchPaths,
}

impl Program {
    pub fn from_settings(
        db: &dyn Db,
        settings: ProgramSettings,
        typeshed_versions_checker: impl FnOnce(
            &dyn Db,
            File,
        )
            -> std::result::Result<(), &TypeshedVersionsParseError>,
    ) -> Result<Self, SearchPathValidationError> {
        let ProgramSettings {
            target_version,
            search_paths,
        } = settings;
        let static_search_paths =
            try_resolve_module_resolution_settings(db, search_paths, typeshed_versions_checker)?;
        Ok(Program::builder(target_version, static_search_paths)
            .durability(Durability::HIGH)
            .new(db))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ProgramSettings {
    pub target_version: TargetVersion,
    pub search_paths: SearchPathSettings,
}

/// Enumeration of all supported Python versions
///
/// TODO: unify with the `PythonVersion` enum in the linter/formatter crates?
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum TargetVersion {
    Py37,
    #[default]
    Py38,
    Py39,
    Py310,
    Py311,
    Py312,
    Py313,
}

impl TargetVersion {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Py37 => "py37",
            Self::Py38 => "py38",
            Self::Py39 => "py39",
            Self::Py310 => "py310",
            Self::Py311 => "py311",
            Self::Py312 => "py312",
            Self::Py313 => "py313",
        }
    }
}

impl std::fmt::Display for TargetVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::fmt::Debug for TargetVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}
