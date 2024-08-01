use std::fmt;

use rustc_hash::{FxBuildHasher, FxHashSet};

use crate::files::{system_path_to_file, File, FileError, FileRootKind};
use crate::system::{System, SystemPath, SystemPathBuf};
use crate::typeshed_version_error::TypeshedVersionsParseError;
use crate::vendored::VendoredPathBuf;
use crate::Db;

/// Enumeration describing the various ways in which validation of a search path might fail.
///
/// If validation fails for a search path derived from the user settings,
/// a message must be displayed to the user,
/// as type checking cannot be done reliably in these circumstances.
#[derive(Debug, PartialEq, Eq)]
pub enum SearchPathValidationError {
    /// The path provided by the user was not a directory
    NotADirectory(SystemPathBuf),

    /// The path provided by the user is a directory,
    /// but no `stdlib/` subdirectory exists.
    /// (This is only relevant for stdlib search paths.)
    NoStdlibSubdirectory(SystemPathBuf),

    /// The typeshed path provided by the user is a directory,
    /// but no `stdlib/VERSIONS` file exists.
    /// (This is only relevant for stdlib search paths.)
    NoVersionsFile(SystemPathBuf),

    /// `stdlib/VERSIONS` is a directory.
    /// (This is only relevant for stdlib search paths.)
    VersionsIsADirectory(SystemPathBuf),

    /// The path provided by the user is a directory,
    /// and a `stdlib/VERSIONS` file exists, but it fails to parse.
    /// (This is only relevant for stdlib search paths.)
    VersionsParseError(TypeshedVersionsParseError),
}

impl fmt::Display for SearchPathValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotADirectory(path) => write!(f, "{path} does not point to a directory"),
            Self::NoStdlibSubdirectory(path) => {
                write!(f, "The directory at {path} has no `stdlib/` subdirectory")
            }
            Self::NoVersionsFile(path) => write!(f, "Expected a file at {path}/stdlib/VERSIONS"),
            Self::VersionsIsADirectory(path) => write!(f, "{path}/stdlib/VERSIONS is a directory."),
            Self::VersionsParseError(underlying_error) => {
                std::fmt::Display::fmt(underlying_error, f)
            }
        }
    }
}

impl std::error::Error for SearchPathValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let Self::VersionsParseError(underlying_error) = self {
            Some(underlying_error)
        } else {
            None
        }
    }
}

type SearchPathResult<T> = Result<T, SearchPathValidationError>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum ValidatedSearchPath {
    Extra(SystemPathBuf),
    FirstParty(SystemPathBuf),
    StandardLibraryCustom(SystemPathBuf),
    StandardLibraryVendored(VendoredPathBuf),
    SitePackages(SystemPathBuf),
    Editable(SystemPathBuf),
}

impl ValidatedSearchPath {
    /// Create a new "Extra" search path
    pub fn extra(system: &dyn System, root: SystemPathBuf) -> SearchPathResult<Self> {
        if system.is_directory(&root) {
            Ok(Self::Extra(root))
        } else {
            Err(SearchPathValidationError::NotADirectory(root))
        }
    }

    /// Create a new first-party search path, pointing to the user code we were directly invoked on
    pub fn first_party(system: &dyn System, root: SystemPathBuf) -> SearchPathResult<Self> {
        if system.is_directory(&root) {
            Ok(Self::FirstParty(root))
        } else {
            Err(SearchPathValidationError::NotADirectory(root))
        }
    }

    /// Create a new standard-library search path pointing to a custom directory on disk
    pub fn custom_stdlib(
        db: &dyn Db,
        typeshed: SystemPathBuf,
        check_versions_fn: impl FnOnce(
            &dyn Db,
            File,
        ) -> std::result::Result<(), &TypeshedVersionsParseError>,
    ) -> SearchPathResult<Self> {
        let system = db.system();
        if !system.is_directory(&typeshed) {
            return Err(SearchPathValidationError::NotADirectory(
                typeshed.to_path_buf(),
            ));
        }
        let stdlib = typeshed.join("stdlib");
        if !system.is_directory(&stdlib) {
            return Err(SearchPathValidationError::NoStdlibSubdirectory(
                typeshed.to_path_buf(),
            ));
        }
        let typeshed_versions =
            system_path_to_file(db, stdlib.join("VERSIONS")).map_err(|err| match err {
                FileError::NotFound => SearchPathValidationError::NoVersionsFile(typeshed),
                FileError::IsADirectory => {
                    SearchPathValidationError::VersionsIsADirectory(typeshed)
                }
            })?;
        check_versions_fn(db, typeshed_versions).map_err(|validation_error| {
            SearchPathValidationError::VersionsParseError(validation_error.clone())
        })?;
        Ok(Self::StandardLibraryCustom(stdlib))
    }

    /// Create a new search path pointing to the `stdlib/` subdirectory in the vendored zip archive
    #[must_use]
    pub fn vendored_stdlib() -> Self {
        Self::StandardLibraryVendored(VendoredPathBuf::from("stdlib"))
    }

    /// Create a new search path pointing to the `site-packages` directory on disk
    pub fn site_packages(system: &dyn System, root: SystemPathBuf) -> SearchPathResult<Self> {
        if system.is_directory(&root) {
            Ok(Self::SitePackages(root))
        } else {
            Err(SearchPathValidationError::NotADirectory(root))
        }
    }

    /// Create a new search path pointing to an editable installation
    pub fn editable(system: &dyn System, root: SystemPathBuf) -> SearchPathResult<Self> {
        if system.is_directory(&root) {
            Ok(Self::Editable(root))
        } else {
            Err(SearchPathValidationError::NotADirectory(root))
        }
    }

    pub fn as_system_path(&self) -> Option<&SystemPath> {
        match self {
            Self::FirstParty(path)
            | Self::Extra(path)
            | Self::Editable(path)
            | Self::SitePackages(path)
            | Self::StandardLibraryCustom(path) => Some(path),
            Self::StandardLibraryVendored(_) => None,
        }
    }
}

/// Configures the search paths for module resolution.
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct SearchPathSettings {
    /// List of user-provided paths that should take first priority in the module resolution.
    /// Examples in other type checkers are mypy's MYPYPATH environment variable,
    /// or pyright's stubPath configuration setting.
    pub extra_paths: Vec<SystemPathBuf>,

    /// The root of the workspace, used for finding first-party modules.
    pub workspace_root: SystemPathBuf,

    /// Optional path to a "custom typeshed" directory on disk for us to use for standard-library types.
    /// If this is not provided, we will fallback to our vendored typeshed stubs for the stdlib,
    /// bundled as a zip file in the binary
    pub custom_typeshed: Option<SystemPathBuf>,

    /// The path to the user's `site-packages` directory, where third-party packages from ``PyPI`` are installed.
    pub site_packages: Vec<SystemPathBuf>,
}

/// Validate and normalize the raw settings given by the user
/// into settings we can use for module resolution
///
/// This method also implements the typing spec's [module resolution order].
///
/// [module resolution order]: https://typing.readthedocs.io/en/latest/spec/distributing.html#import-resolution-ordering
pub(crate) fn try_resolve_module_resolution_settings(
    db: &dyn Db,
    raw_search_path_settings: SearchPathSettings,
    typeshed_versions_checker: impl FnOnce(
        &dyn Db,
        File,
    ) -> std::result::Result<(), &TypeshedVersionsParseError>,
) -> Result<StaticSearchPaths, SearchPathValidationError> {
    let SearchPathSettings {
        extra_paths,
        workspace_root,
        custom_typeshed,
        site_packages,
    } = raw_search_path_settings;

    if let Some(custom_typeshed) = &custom_typeshed {
        tracing::info!("Custom typeshed directory: {custom_typeshed}");
    }

    if !extra_paths.is_empty() {
        tracing::info!("extra search paths: {extra_paths:?}");
    }

    let system = db.system();
    let files = db.files();

    let mut static_search_paths = vec![];

    for path in extra_paths {
        files.try_add_root(db, &path, FileRootKind::LibrarySearchPath);
        static_search_paths.push(ValidatedSearchPath::extra(system, path.clone())?);
    }

    static_search_paths.push(ValidatedSearchPath::first_party(
        system,
        workspace_root.clone(),
    )?);

    static_search_paths.push(if let Some(custom_typeshed) = custom_typeshed.as_ref() {
        files.try_add_root(db, custom_typeshed, FileRootKind::LibrarySearchPath);
        ValidatedSearchPath::custom_stdlib(db, custom_typeshed.clone(), typeshed_versions_checker)?
    } else {
        ValidatedSearchPath::vendored_stdlib()
    });

    for site_packages_dir in site_packages {
        files.try_add_root(db, &site_packages_dir, FileRootKind::LibrarySearchPath);

        static_search_paths.push(ValidatedSearchPath::site_packages(
            system,
            site_packages_dir.clone(),
        )?);
    }

    // TODO vendor typeshed's third-party stubs as well as the stdlib and fallback to them as a final step

    // Filter out module resolution paths that point to the same directory on disk (the same invariant maintained by [`sys.path` at runtime]).
    // (Paths may, however, *overlap* -- e.g. you could have both `src/` and `src/foo`
    // as module resolution paths simultaneously.)
    //
    // [`sys.path` at runtime]: https://docs.python.org/3/library/site.html#module-site
    // This code doesn't use an `IndexSet` because the key is the system path and not the search root.
    let mut seen_paths =
        FxHashSet::with_capacity_and_hasher(static_search_paths.len(), FxBuildHasher);

    static_search_paths.retain(|path| {
        if let Some(path) = path.as_system_path() {
            seen_paths.insert(path.to_path_buf())
        } else {
            true
        }
    });

    Ok(StaticSearchPaths(static_search_paths))
}

/// Search paths that have been statically determined purely from reading Ruff's configuration settings.
/// These shouldn't ever change unless the config settings themselves change.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StaticSearchPaths(Vec<ValidatedSearchPath>);
