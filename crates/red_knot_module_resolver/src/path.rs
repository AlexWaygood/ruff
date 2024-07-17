//! Internal abstractions for differentiating between different kinds of search paths,
//! and paths relative to those search paths.

use std::borrow::Cow;
use std::fmt;
use std::io;

use ruff_db::files::{system_path_to_file, vendored_path_to_file, File, FilePath};
use ruff_db::system::{System, SystemPath, SystemPathBuf};
use ruff_db::vendored::{VendoredPath, VendoredPathBuf};

use crate::db::Db;
use crate::module_name::ModuleName;
use crate::state::ResolverState;
use crate::typeshed::TypeshedVersionsQueryResult;
use crate::TypeshedVersionsParseError;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RelativeSystemPathBuf {
    root: SystemPathBuf,
    path: SystemPathBuf,
}

impl RelativeSystemPathBuf {
    #[must_use]
    fn root_path(root: SystemPathBuf) -> Self {
        Self {
            root,
            path: SystemPathBuf::new(),
        }
    }

    fn push(&mut self, component: impl AsRef<SystemPath>) {
        let component = component.as_ref();
        let RelativeSystemPathBuf { root: _, path } = self;
        assert!(
            path.extension().is_none() || component.extension().is_none(),
            "Cannot push part {component} to {path:?}, which already has an extension"
        );
        path.push(component);
    }

    fn with_extension(&self, extension: &str) -> RelativeSystemPathRef {
        let RelativeSystemPathBuf { root, path } = self;
        RelativeSystemPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Owned(path.with_extension(extension)),
        }
    }

    fn relativize_path<'a>(&'a self, path: &'a SystemPathBuf) -> Option<RelativeSystemPathRef<'a>> {
        let RelativeSystemPathBuf { root, path: tail } = self;
        assert_eq!(
            &**tail,
            SystemPath::new(""),
            "`.relativize_path()` should only be called on a module search path root"
        );
        Some(RelativeSystemPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Borrowed(path.strip_prefix(root).ok()?),
        })
    }
}

impl From<&RelativeSystemPathBuf> for SystemPathBuf {
    fn from(value: &RelativeSystemPathBuf) -> Self {
        let RelativeSystemPathBuf { root, path } = value;
        root.join(path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RelativeSystemPathRef<'a> {
    root: Cow<'a, SystemPath>,
    path: Cow<'a, SystemPath>,
}

impl<'a> RelativeSystemPathRef<'a> {
    fn to_file(&self, db: &dyn Db) -> Option<File> {
        system_path_to_file(db.upcast(), SystemPathBuf::from(self))
    }
}

impl<'a> From<&'a RelativeSystemPathBuf> for RelativeSystemPathRef<'a> {
    #[inline]
    fn from(value: &'a RelativeSystemPathBuf) -> Self {
        let RelativeSystemPathBuf { root, path } = value;
        RelativeSystemPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Borrowed(path),
        }
    }
}

impl<'a> From<&'a RelativeSystemPathRef<'a>> for SystemPathBuf {
    fn from(value: &RelativeSystemPathRef) -> Self {
        let RelativeSystemPathRef { root, path } = value;
        root.join(path)
    }
}

impl PartialEq<SystemPath> for RelativeSystemPathRef<'_> {
    fn eq(&self, other: &SystemPath) -> bool {
        let RelativeSystemPathRef { root, path } = self;
        other.starts_with(root)
            && other.ends_with(path)
            && other.components().count() == root.components().count() + path.components().count()
    }
}

impl PartialEq<RelativeSystemPathRef<'_>> for SystemPath {
    fn eq(&self, other: &RelativeSystemPathRef<'_>) -> bool {
        other.eq(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RelativeVendoredPathBuf {
    root: VendoredPathBuf,
    path: VendoredPathBuf,
}

impl RelativeVendoredPathBuf {
    fn root_path(root: VendoredPathBuf) -> Self {
        Self {
            root,
            path: VendoredPathBuf::new(),
        }
    }

    fn push(&mut self, component: impl AsRef<VendoredPath>) {
        let component = component.as_ref();
        let RelativeVendoredPathBuf { root: _, path } = self;
        assert!(
            path.extension().is_none() || component.extension().is_none(),
            "Cannot push part {component:?} to {path:?}, which already has an extension"
        );
        path.push(component);
    }

    fn with_pyi_extension(&self) -> RelativeVendoredPathRef {
        let RelativeVendoredPathBuf { root, path } = self;
        RelativeVendoredPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Owned(path.with_pyi_extension()),
        }
    }

    fn relativize_path<'a>(
        &'a self,
        path: &'a VendoredPathBuf,
    ) -> Option<RelativeVendoredPathRef<'a>> {
        let RelativeVendoredPathBuf { root, path: tail } = self;
        assert_eq!(
            &**tail,
            VendoredPath::new(""),
            "`.relativize_path()` should only be called on a module search path root"
        );
        Some(RelativeVendoredPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Borrowed(path.strip_prefix(root).ok()?),
        })
    }
}

impl From<&RelativeVendoredPathBuf> for VendoredPathBuf {
    fn from(value: &RelativeVendoredPathBuf) -> Self {
        let RelativeVendoredPathBuf { root, path } = value;
        root.join(path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct RelativeVendoredPathRef<'a> {
    root: Cow<'a, VendoredPath>,
    path: Cow<'a, VendoredPath>,
}

impl<'a> RelativeVendoredPathRef<'a> {
    fn to_file(&self, db: &dyn Db) -> Option<File> {
        vendored_path_to_file(db.upcast(), VendoredPathBuf::from(self))
    }
}

impl<'a> From<&'a RelativeVendoredPathBuf> for RelativeVendoredPathRef<'a> {
    fn from(value: &'a RelativeVendoredPathBuf) -> Self {
        let RelativeVendoredPathBuf { root, path } = value;
        RelativeVendoredPathRef {
            root: Cow::Borrowed(root),
            path: Cow::Borrowed(path),
        }
    }
}

impl<'a> From<&'a RelativeVendoredPathRef<'a>> for VendoredPathBuf {
    fn from(value: &'a RelativeVendoredPathRef<'a>) -> Self {
        let RelativeVendoredPathRef { root, path } = value;
        root.join(path)
    }
}

impl PartialEq<VendoredPath> for RelativeVendoredPathRef<'_> {
    fn eq(&self, other: &VendoredPath) -> bool {
        let RelativeVendoredPathRef { root, path } = self;
        other.starts_with(root)
            && other.ends_with(path)
            && other.components().count() == root.components().count() + path.components().count()
    }
}

impl PartialEq<RelativeVendoredPathRef<'_>> for VendoredPath {
    fn eq(&self, other: &RelativeVendoredPathRef<'_>) -> bool {
        other.eq(self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum AnyRelativePathBuf {
    System(RelativeSystemPathBuf),
    Vendored(RelativeVendoredPathBuf),
}

impl fmt::Debug for AnyRelativePathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::System(path) => f
                .debug_tuple("AnyRelativePathBuf::System")
                .field(&SystemPathBuf::from(path))
                .finish(),
            Self::Vendored(path) => f
                .debug_tuple("AnyRelativePathBuf::Vendored")
                .field(&VendoredPathBuf::from(path))
                .finish(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AnyRelativePathRef<'a> {
    System(RelativeSystemPathRef<'a>),
    Vendored(RelativeVendoredPathRef<'a>),
}

impl<'a> AnyRelativePathRef<'a> {
    fn to_file(&self, db: &dyn Db) -> Option<File> {
        match self {
            Self::System(fs_path) => fs_path.to_file(db),
            Self::Vendored(vendored_path) => vendored_path.to_file(db),
        }
    }
}

impl<'a> From<&'a AnyRelativePathBuf> for AnyRelativePathRef<'a> {
    #[inline]
    fn from(value: &'a AnyRelativePathBuf) -> Self {
        match value {
            AnyRelativePathBuf::System(path) => {
                AnyRelativePathRef::System(RelativeSystemPathRef::from(path))
            }
            AnyRelativePathBuf::Vendored(path) => {
                AnyRelativePathRef::Vendored(RelativeVendoredPathRef::from(path))
            }
        }
    }
}

/// Enumeration of the different kinds of search paths type checkers are expected to support.
///
/// N.B. Although we don't implement `Ord` for this enum, they are ordered in terms of the
/// priority that we want to give these modules when resolving them,
/// as per [the order given in the typing spec]
///
/// [the order given in the typing spec]: https://typing.readthedocs.io/en/latest/spec/distributing.html#import-resolution-ordering
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ModuleResolutionPathBufInner {
    Extra(RelativeSystemPathBuf),
    FirstParty(RelativeSystemPathBuf),
    StandardLibrary(AnyRelativePathBuf),
    SitePackages(RelativeSystemPathBuf),
    EditableInstall(RelativeSystemPathBuf),
}

impl ModuleResolutionPathBufInner {
    /// Push a new part to the path,
    /// while maintaining the invariant that the path can only have `.py` or `.pyi` extensions.
    /// For the stdlib variant specifically, it may only have a `.pyi` extension.
    ///
    /// # Panics:
    /// If a component with an invalid extension is passed
    fn push(&mut self, component: &str) {
        let extension = camino::Utf8Path::new(component).extension();
        match self {
            Self::Extra(ref mut path) => {
                assert!(
                    matches!(extension, Some("pyi" | "py") | None),
                    "Extension must be `py` or `pyi`; got `{}`",
                    extension.unwrap()
                );
                path.push(component);
            }
            Self::FirstParty(ref mut path) => {
                assert!(
                    matches!(extension, Some("pyi" | "py") | None),
                    "Extension must be `py` or `pyi`; got `{}`",
                    extension.unwrap()
                );
                path.push(component);
            }
            Self::StandardLibrary(ref mut path) => {
                assert!(
                    matches!(extension, Some("pyi") | None),
                    "Extension must be `pyi`; got `{}`",
                    extension.unwrap()
                );
                match path {
                    AnyRelativePathBuf::System(path) => path.push(component),
                    AnyRelativePathBuf::Vendored(path) => path.push(component),
                }
            }
            Self::SitePackages(ref mut path) => {
                assert!(
                    matches!(extension, Some("pyi" | "py") | None),
                    "Extension must be `py` or `pyi`; got `{}`",
                    extension.unwrap()
                );
                path.push(component);
            }
            Self::EditableInstall(ref mut path) => {
                assert!(
                    matches!(extension, Some("pyi" | "py") | None),
                    "Extension must be `py` or `pyi`; got `{}`",
                    extension.unwrap()
                );
                path.push(component);
            }
        }
    }

    fn with_pyi_extension(&self) -> ModuleResolutionPathRefInner {
        match self {
            Self::Extra(path) => ModuleResolutionPathRefInner::Extra(path.with_extension("pyi")),
            Self::FirstParty(path) => {
                ModuleResolutionPathRefInner::FirstParty(path.with_extension("pyi"))
            }
            Self::SitePackages(path) => {
                ModuleResolutionPathRefInner::SitePackages(path.with_extension("pyi"))
            }
            Self::EditableInstall(path) => {
                ModuleResolutionPathRefInner::EditableInstall(path.with_extension("pyi"))
            }
            Self::StandardLibrary(AnyRelativePathBuf::System(path)) => {
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(
                    path.with_extension("pyi"),
                ))
            }
            Self::StandardLibrary(AnyRelativePathBuf::Vendored(path)) => {
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(
                    path.with_pyi_extension(),
                ))
            }
        }
    }

    fn with_py_extension(&self) -> Option<ModuleResolutionPathRefInner> {
        let inner = match self {
            Self::Extra(path) => ModuleResolutionPathRefInner::Extra(path.with_extension("py")),
            Self::FirstParty(path) => {
                ModuleResolutionPathRefInner::FirstParty(path.with_extension("py"))
            }
            Self::SitePackages(path) => {
                ModuleResolutionPathRefInner::SitePackages(path.with_extension("py"))
            }
            Self::EditableInstall(path) => {
                ModuleResolutionPathRefInner::EditableInstall(path.with_extension("py"))
            }
            Self::StandardLibrary(_) => return None,
        };
        Some(inner)
    }

    fn relativize_path<'a>(
        &'a self,
        path: &'a FilePath,
    ) -> Option<ModuleResolutionPathRefInner<'a>> {
        match (self, path) {
            (Self::Extra(root), FilePath::System(path)) => {
                if matches!(path.extension(), Some("pyi" | "py") | None) {
                    root.relativize_path(path)
                        .map(ModuleResolutionPathRefInner::Extra)
                } else {
                    None
                }
            }
            (Self::Extra(_), FilePath::Vendored(_)) => None,
            (Self::FirstParty(root), FilePath::System(path)) => {
                if matches!(path.extension(), Some("pyi" | "py") | None) {
                    root.relativize_path(path)
                        .map(ModuleResolutionPathRefInner::FirstParty)
                } else {
                    None
                }
            }
            (Self::FirstParty(_), FilePath::Vendored(_)) => None,
            (Self::StandardLibrary(AnyRelativePathBuf::System(root)), FilePath::System(path)) => {
                if matches!(path.extension(), Some("pyi") | None) {
                    root.relativize_path(path).map(|path| {
                        ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(
                            path,
                        ))
                    })
                } else {
                    None
                }
            }
            (Self::StandardLibrary(AnyRelativePathBuf::System(_)), FilePath::Vendored(_)) => None,
            (
                Self::StandardLibrary(AnyRelativePathBuf::Vendored(root)),
                FilePath::Vendored(path),
            ) => {
                if matches!(path.extension(), Some("pyi") | None) {
                    root.relativize_path(path).map(|path| {
                        ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(
                            path,
                        ))
                    })
                } else {
                    None
                }
            }
            (Self::StandardLibrary(AnyRelativePathBuf::Vendored(_)), FilePath::System(_)) => None,
            (Self::SitePackages(root), FilePath::System(path)) => {
                if matches!(path.extension(), Some("py" | "pyi") | None) {
                    root.relativize_path(path)
                        .map(ModuleResolutionPathRefInner::SitePackages)
                } else {
                    None
                }
            }
            (Self::SitePackages(_), FilePath::Vendored(_)) => None,
            (Self::EditableInstall(root), FilePath::System(path)) => {
                if matches!(path.extension(), Some("py" | "pyi") | None) {
                    root.relativize_path(path)
                        .map(ModuleResolutionPathRefInner::EditableInstall)
                } else {
                    None
                }
            }
            (Self::EditableInstall(_), FilePath::Vendored(_)) => None,
        }
    }

    #[must_use]
    fn is_directory(&self, resolver: &ResolverState) -> bool {
        match self {
            Self::Extra(path) => resolver.system().is_directory(&SystemPathBuf::from(path)),
            Self::FirstParty(path) => resolver.system().is_directory(&SystemPathBuf::from(path)),
            Self::SitePackages(path) => resolver.system().is_directory(&SystemPathBuf::from(path)),
            Self::EditableInstall(path) => {
                resolver.system().is_directory(&SystemPathBuf::from(path))
            }
            Self::StandardLibrary(path) => {
                match query_stdlib_version(&AnyRelativePathRef::from(path), resolver) {
                    TypeshedVersionsQueryResult::DoesNotExist => false,
                    TypeshedVersionsQueryResult::Exists
                    | TypeshedVersionsQueryResult::MaybeExists => match path {
                        AnyRelativePathBuf::System(path) => {
                            resolver.system().is_directory(&SystemPathBuf::from(path))
                        }
                        AnyRelativePathBuf::Vendored(path) => resolver
                            .vendored()
                            .is_directory(VendoredPathBuf::from(path)),
                    },
                }
            }
        }
    }

    #[must_use]
    fn is_regular_package(&self, resolver: &ResolverState) -> bool {
        fn is_non_stdlib_pkg(resolver: &ResolverState, path: &RelativeSystemPathBuf) -> bool {
            let file_system = resolver.system();
            let fs_path = SystemPathBuf::from(path);
            file_system.path_exists(&fs_path.join("__init__.py"))
                || file_system.path_exists(&fs_path.join("__init__.pyi"))
        }

        match self {
            Self::Extra(path) => is_non_stdlib_pkg(resolver, path),
            Self::FirstParty(path) => is_non_stdlib_pkg(resolver, path),
            Self::SitePackages(path) => is_non_stdlib_pkg(resolver, path),
            Self::EditableInstall(path) => is_non_stdlib_pkg(resolver, path),
            // Unlike the other variants:
            // (1) Account for VERSIONS
            // (2) Only test for `__init__.pyi`, not `__init__.py`
            Self::StandardLibrary(path) => {
                match query_stdlib_version(&AnyRelativePathRef::from(path), resolver) {
                    TypeshedVersionsQueryResult::DoesNotExist => false,
                    TypeshedVersionsQueryResult::Exists
                    | TypeshedVersionsQueryResult::MaybeExists => match path {
                        AnyRelativePathBuf::System(path) => resolver
                            .db
                            .system()
                            .path_exists(&SystemPathBuf::from(path).join("__init__.pyi")),
                        AnyRelativePathBuf::Vendored(path) => resolver
                            .db
                            .vendored()
                            .exists(VendoredPathBuf::from(path).join("__init__.pyi")),
                    },
                }
            }
        }
    }
}

fn not_a_directory() -> io::Error {
    std::io::Error::new(std::io::ErrorKind::Other, "Not a directory")
}

#[derive(Debug, PartialEq, Eq)]
pub enum CustomTypeshedValidationError {
    NotADirectory(SystemPathBuf),
    NoStdlibSubdirectory(SystemPathBuf),
    NoVersionsFile(SystemPathBuf),
    VersionsParseError(TypeshedVersionsParseError),
}

impl fmt::Display for CustomTypeshedValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotADirectory(path) => write!(f, "{path} does not point to a directory"),
            Self::NoStdlibSubdirectory(path) => {
                write!(f, "The directory at {path} has no `stdlib/` subdirectory")
            }
            Self::NoVersionsFile(path) => write!(f, "Expected a file at {path}/stldib/VERSIONS"),
            Self::VersionsParseError(underlying_error) => underlying_error.fmt(f),
        }
    }
}

impl std::error::Error for CustomTypeshedValidationError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        if let Self::VersionsParseError(underlying_error) = self {
            Some(underlying_error)
        } else {
            None
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) struct ModuleResolutionPathBuf(ModuleResolutionPathBufInner);

impl ModuleResolutionPathBuf {
    pub(crate) fn extra_search_path(
        system: &dyn System,
        root: impl Into<SystemPathBuf>,
    ) -> io::Result<Self> {
        let root = root.into();
        if system.is_directory(&root) {
            Ok(Self(ModuleResolutionPathBufInner::Extra(
                RelativeSystemPathBuf::root_path(root),
            )))
        } else {
            Err(not_a_directory())
        }
    }

    pub(crate) fn first_party_search_path(
        system: &dyn System,
        root: impl Into<SystemPathBuf>,
    ) -> io::Result<Self> {
        let root = root.into();
        if system.is_directory(&root) {
            Ok(Self(ModuleResolutionPathBufInner::FirstParty(
                RelativeSystemPathBuf::root_path(root),
            )))
        } else {
            Err(not_a_directory())
        }
    }

    pub(crate) fn custom_stdlib_search_path(
        db: &dyn Db,
        typeshed: impl Into<SystemPathBuf>,
    ) -> Result<Self, CustomTypeshedValidationError> {
        let typeshed = typeshed.into();
        let system = db.system();
        if !system.is_directory(&typeshed) {
            return Err(CustomTypeshedValidationError::NotADirectory(typeshed));
        }
        let stdlib = typeshed.join("stdlib");
        if !system.is_directory(&stdlib) {
            return Err(CustomTypeshedValidationError::NoStdlibSubdirectory(
                typeshed,
            ));
        }
        let Some(typeshed_versions) = system_path_to_file(db.upcast(), stdlib.join("VERSIONS"))
        else {
            return Err(CustomTypeshedValidationError::NoVersionsFile(typeshed));
        };
        crate::typeshed::parse_typeshed_versions(db, typeshed_versions)
            .as_ref()
            .map_err(|validation_error| {
                CustomTypeshedValidationError::VersionsParseError(validation_error.clone())
            })?;
        Ok(Self(ModuleResolutionPathBufInner::StandardLibrary(
            AnyRelativePathBuf::System(RelativeSystemPathBuf::root_path(stdlib)),
        )))
    }

    #[must_use]
    pub(crate) fn vendored_stdlib() -> Self {
        Self(ModuleResolutionPathBufInner::StandardLibrary(
            AnyRelativePathBuf::Vendored(RelativeVendoredPathBuf::root_path(
                VendoredPathBuf::from("stdlib"),
            )),
        ))
    }

    pub(crate) fn site_packages_search_path(
        system: &dyn System,
        root: impl Into<SystemPathBuf>,
    ) -> io::Result<Self> {
        let root = root.into();
        if system.is_directory(&root) {
            Ok(Self(ModuleResolutionPathBufInner::SitePackages(
                RelativeSystemPathBuf::root_path(root),
            )))
        } else {
            Err(not_a_directory())
        }
    }

    pub(crate) fn editable_install_search_path(
        system: &dyn System,
        root: impl Into<SystemPathBuf>,
    ) -> io::Result<Self> {
        let root = root.into();
        if system.is_directory(&root) {
            Ok(Self(ModuleResolutionPathBufInner::EditableInstall(
                RelativeSystemPathBuf::root_path(root),
            )))
        } else {
            Err(not_a_directory())
        }
    }

    pub(crate) fn push(&mut self, component: &str) {
        self.0.push(component);
    }

    #[must_use]
    pub(crate) fn is_regular_package(&self, resolver: &ResolverState) -> bool {
        self.0.is_regular_package(resolver)
    }

    #[must_use]
    pub(crate) fn is_directory(&self, resolver: &ResolverState) -> bool {
        self.0.is_directory(resolver)
    }

    #[must_use]
    pub(crate) const fn is_site_packages(&self) -> bool {
        matches!(self.0, ModuleResolutionPathBufInner::SitePackages(_))
    }

    #[must_use]
    pub(crate) fn with_pyi_extension(&self) -> ModuleResolutionPathRef {
        ModuleResolutionPathRef(self.0.with_pyi_extension())
    }

    #[must_use]
    pub(crate) fn with_py_extension(&self) -> Option<ModuleResolutionPathRef> {
        self.0.with_py_extension().map(ModuleResolutionPathRef)
    }

    #[must_use]
    pub(crate) fn relativize_path<'a>(
        &'a self,
        path: &'a FilePath,
    ) -> Option<ModuleResolutionPathRef<'a>> {
        self.0.relativize_path(path).map(ModuleResolutionPathRef)
    }

    pub(crate) fn to_system_path_buf(&self) -> Option<SystemPathBuf> {
        let relative_system_path = match &self.0 {
            ModuleResolutionPathBufInner::Extra(extra_path) => extra_path,
            ModuleResolutionPathBufInner::FirstParty(first_party_path) => first_party_path,
            ModuleResolutionPathBufInner::StandardLibrary(_) => return None,
            ModuleResolutionPathBufInner::SitePackages(site_packages_path) => site_packages_path,
            ModuleResolutionPathBufInner::EditableInstall(editable_path) => editable_path,
        };
        Some(SystemPathBuf::from(relative_system_path))
    }
}

impl fmt::Debug for ModuleResolutionPathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            ModuleResolutionPathBufInner::Extra(path) => f
                .debug_tuple("ModuleResolutionPathBuf::Extra")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathBufInner::FirstParty(path) => f
                .debug_tuple("ModuleResolutionPathBuf::FirstParty")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathBufInner::SitePackages(path) => f
                .debug_tuple("ModuleResolutionPathBuf::SitePackages")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathBufInner::StandardLibrary(path) => f
                .debug_tuple("ModuleResolutionPathBuf::StandardLibrary")
                .field(path)
                .finish(),
            ModuleResolutionPathBufInner::EditableInstall(path) => f
                .debug_tuple("ModuleResolutionPathBuf::EditableInstall")
                .field(&SystemPathBuf::from(path))
                .finish(),
        }
    }
}

impl PartialEq<SystemPathBuf> for ModuleResolutionPathBuf {
    fn eq(&self, other: &SystemPathBuf) -> bool {
        ModuleResolutionPathRef::from(self) == **other
    }
}

impl PartialEq<ModuleResolutionPathBuf> for SystemPathBuf {
    fn eq(&self, other: &ModuleResolutionPathBuf) -> bool {
        other.eq(self)
    }
}

impl PartialEq<VendoredPathBuf> for ModuleResolutionPathBuf {
    fn eq(&self, other: &VendoredPathBuf) -> bool {
        ModuleResolutionPathRef::from(self) == **other
    }
}

impl PartialEq<ModuleResolutionPathBuf> for VendoredPathBuf {
    fn eq(&self, other: &ModuleResolutionPathBuf) -> bool {
        other.eq(self)
    }
}

#[must_use]
fn stdlib_path_to_module_name(stdlib_path: &AnyRelativePathRef) -> Option<ModuleName> {
    match stdlib_path {
        AnyRelativePathRef::System(RelativeSystemPathRef { root: _, path }) => {
            let parent_components = path
                .parent()?
                .components()
                .map(|component| component.as_str());
            let skip_final_part = path.ends_with("__init__.pyi");
            if skip_final_part {
                ModuleName::from_components(parent_components)
            } else {
                ModuleName::from_components(parent_components.chain(path.file_stem()))
            }
        }
        AnyRelativePathRef::Vendored(RelativeVendoredPathRef { root: _, path }) => {
            let parent_components = path
                .parent()?
                .components()
                .map(|component| component.as_str());
            let skip_final_part = path.ends_with("__init__.pyi");
            if skip_final_part {
                ModuleName::from_components(parent_components)
            } else {
                ModuleName::from_components(parent_components.chain(path.file_stem()))
            }
        }
    }
}

#[must_use]
fn query_stdlib_version(
    stdlib_path: &AnyRelativePathRef,
    resolver: &ResolverState,
) -> TypeshedVersionsQueryResult {
    let Some(module_name) = stdlib_path_to_module_name(stdlib_path) else {
        return TypeshedVersionsQueryResult::DoesNotExist;
    };
    let ResolverState {
        db,
        typeshed_versions,
        target_version,
    } = resolver;
    let root_to_pass = match stdlib_path {
        AnyRelativePathRef::System(fs_path) => Some(&*fs_path.root),
        AnyRelativePathRef::Vendored(_) => None,
    };
    typeshed_versions.query_module(*db, &module_name, root_to_pass, *target_version)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum ModuleResolutionPathRefInner<'a> {
    Extra(RelativeSystemPathRef<'a>),
    FirstParty(RelativeSystemPathRef<'a>),
    StandardLibrary(AnyRelativePathRef<'a>),
    SitePackages(RelativeSystemPathRef<'a>),
    EditableInstall(RelativeSystemPathRef<'a>),
}

impl<'a> ModuleResolutionPathRefInner<'a> {
    fn to_file(&self, resolver: &ResolverState) -> Option<File> {
        let db = resolver.db;
        match self {
            Self::Extra(path) => path.to_file(db),
            Self::FirstParty(path) => path.to_file(db),
            Self::SitePackages(path) => path.to_file(db),
            Self::EditableInstall(path) => path.to_file(db),
            Self::StandardLibrary(path) => match query_stdlib_version(path, resolver) {
                TypeshedVersionsQueryResult::DoesNotExist => None,
                TypeshedVersionsQueryResult::Exists => path.to_file(db),
                TypeshedVersionsQueryResult::MaybeExists => path.to_file(db),
            },
        }
    }

    #[must_use]
    fn to_module_name(&self) -> Option<ModuleName> {
        match self {
            Self::Extra(path)
            | Self::FirstParty(path)
            | Self::SitePackages(path)
            | Self::EditableInstall(path) => {
                let RelativeSystemPathRef { root: _, path } = path;
                let parent_components = path
                    .parent()?
                    .components()
                    .map(|component| component.as_str());
                let skip_final_part =
                    path.ends_with("__init__.py") || path.ends_with("__init__.pyi");
                if skip_final_part {
                    ModuleName::from_components(parent_components)
                } else {
                    ModuleName::from_components(parent_components.chain(path.file_stem()))
                }
            }
            Self::StandardLibrary(path) => stdlib_path_to_module_name(path),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub(crate) struct ModuleResolutionPathRef<'a>(ModuleResolutionPathRefInner<'a>);

impl<'a> ModuleResolutionPathRef<'a> {
    #[must_use]
    pub(crate) fn to_file(&self, resolver: &ResolverState) -> Option<File> {
        self.0.to_file(resolver)
    }

    #[must_use]
    pub(crate) fn to_module_name(&self) -> Option<ModuleName> {
        self.0.to_module_name()
    }
}

impl fmt::Debug for ModuleResolutionPathRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            ModuleResolutionPathRefInner::Extra(path) => f
                .debug_tuple("ModuleResolutionPathRef::Extra")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathRefInner::FirstParty(path) => f
                .debug_tuple("ModuleResolutionPathRef::FirstParty")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathRefInner::SitePackages(path) => f
                .debug_tuple("ModuleResolutionPathRef::SitePackages")
                .field(&SystemPathBuf::from(path))
                .finish(),
            ModuleResolutionPathRefInner::StandardLibrary(path) => f
                .debug_tuple("ModuleResolutionPathRef::StandardLibrary")
                .field(path)
                .finish(),
            ModuleResolutionPathRefInner::EditableInstall(path) => f
                .debug_tuple("ModuleResolutionPathRef::EditableInstall")
                .field(&SystemPathBuf::from(path))
                .finish(),
        }
    }
}

impl<'a> From<&'a ModuleResolutionPathBuf> for ModuleResolutionPathRef<'a> {
    fn from(value: &'a ModuleResolutionPathBuf) -> Self {
        let inner = match &value.0 {
            ModuleResolutionPathBufInner::Extra(path) => {
                ModuleResolutionPathRefInner::Extra(RelativeSystemPathRef::from(path))
            }
            ModuleResolutionPathBufInner::FirstParty(path) => {
                ModuleResolutionPathRefInner::FirstParty(RelativeSystemPathRef::from(path))
            }
            ModuleResolutionPathBufInner::StandardLibrary(AnyRelativePathBuf::System(path)) => {
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(
                    RelativeSystemPathRef::from(path),
                ))
            }
            ModuleResolutionPathBufInner::StandardLibrary(AnyRelativePathBuf::Vendored(path)) => {
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(
                    RelativeVendoredPathRef::from(path),
                ))
            }
            ModuleResolutionPathBufInner::SitePackages(path) => {
                ModuleResolutionPathRefInner::SitePackages(RelativeSystemPathRef::from(path))
            }
            ModuleResolutionPathBufInner::EditableInstall(path) => {
                ModuleResolutionPathRefInner::EditableInstall(RelativeSystemPathRef::from(path))
            }
        };
        ModuleResolutionPathRef(inner)
    }
}

impl PartialEq<SystemPath> for ModuleResolutionPathRef<'_> {
    fn eq(&self, other: &SystemPath) -> bool {
        match &self.0 {
            ModuleResolutionPathRefInner::Extra(path) => path == other,
            ModuleResolutionPathRefInner::FirstParty(path) => path == other,
            ModuleResolutionPathRefInner::SitePackages(path) => path == other,
            ModuleResolutionPathRefInner::EditableInstall(path) => path == other,
            ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(path)) => {
                path == other
            }
            ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(_)) => false,
        }
    }
}

impl PartialEq<ModuleResolutionPathRef<'_>> for SystemPath {
    fn eq(&self, other: &ModuleResolutionPathRef) -> bool {
        other == self
    }
}

impl PartialEq<SystemPathBuf> for ModuleResolutionPathRef<'_> {
    fn eq(&self, other: &SystemPathBuf) -> bool {
        self == &**other
    }
}

impl PartialEq<ModuleResolutionPathRef<'_>> for SystemPathBuf {
    fn eq(&self, other: &ModuleResolutionPathRef<'_>) -> bool {
        &**self == other
    }
}

impl PartialEq<VendoredPath> for ModuleResolutionPathRef<'_> {
    fn eq(&self, other: &VendoredPath) -> bool {
        match &self.0 {
            ModuleResolutionPathRefInner::Extra(_) => false,
            ModuleResolutionPathRefInner::FirstParty(_) => false,
            ModuleResolutionPathRefInner::SitePackages(_) => false,
            ModuleResolutionPathRefInner::EditableInstall(_) => false,
            ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(_)) => false,
            ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(path)) => {
                path == other
            }
        }
    }
}

impl PartialEq<ModuleResolutionPathRef<'_>> for VendoredPath {
    fn eq(&self, other: &ModuleResolutionPathRef) -> bool {
        other == self
    }
}

impl PartialEq<VendoredPathBuf> for ModuleResolutionPathRef<'_> {
    fn eq(&self, other: &VendoredPathBuf) -> bool {
        self == &**other
    }
}

impl PartialEq<ModuleResolutionPathRef<'_>> for VendoredPathBuf {
    fn eq(&self, other: &ModuleResolutionPathRef<'_>) -> bool {
        &**self == other
    }
}

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;
    use ruff_db::program::TargetVersion;
    use ruff_db::Db;

    use crate::db::tests::TestDb;
    use crate::testing::{FileSpec, MockedTypeshed, TestCase, TestCaseBuilder};

    use super::*;

    impl<'a> RelativeSystemPathRef<'a> {
        fn to_path_buf(&self) -> RelativeSystemPathBuf {
            let RelativeSystemPathRef { root, path } = self;
            RelativeSystemPathBuf {
                root: root.to_path_buf(),
                path: path.to_path_buf(),
            }
        }
    }

    impl<'a> RelativeVendoredPathRef<'a> {
        fn to_path_buf(&self) -> RelativeVendoredPathBuf {
            let RelativeVendoredPathRef { root, path } = self;
            RelativeVendoredPathBuf {
                root: root.to_path_buf(),
                path: path.to_path_buf(),
            }
        }
    }

    impl ModuleResolutionPathBuf {
        #[must_use]
        pub(crate) fn join(&self, component: &str) -> Self {
            ModuleResolutionPathRef::from(self).join(component)
        }

        #[must_use]
        pub(crate) fn to_file(&self, resolver: &ResolverState) -> Option<File> {
            ModuleResolutionPathRef::from(self).to_file(resolver)
        }

        #[must_use]
        pub(crate) fn to_module_name(&self) -> Option<ModuleName> {
            ModuleResolutionPathRef::from(self).to_module_name()
        }

        #[must_use]
        pub(crate) const fn is_stdlib_search_path(&self) -> bool {
            matches!(&self.0, ModuleResolutionPathBufInner::StandardLibrary(_))
        }
    }

    impl<'a> ModuleResolutionPathRef<'a> {
        #[must_use]
        fn join(
            &self,
            component: &'a (impl AsRef<SystemPath> + ?Sized),
        ) -> ModuleResolutionPathBuf {
            let mut result = self.clone().to_path_buf();
            result.push(component.as_ref().as_str());
            result
        }

        #[must_use]
        pub(crate) fn to_path_buf(&self) -> ModuleResolutionPathBuf {
            let inner = match &self.0 {
                ModuleResolutionPathRefInner::Extra(path) => {
                    ModuleResolutionPathBufInner::Extra(path.to_path_buf())
                }
                ModuleResolutionPathRefInner::FirstParty(path) => {
                    ModuleResolutionPathBufInner::FirstParty(path.to_path_buf())
                }
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::System(path)) => {
                    ModuleResolutionPathBufInner::StandardLibrary(AnyRelativePathBuf::System(
                        path.to_path_buf(),
                    ))
                }
                ModuleResolutionPathRefInner::StandardLibrary(AnyRelativePathRef::Vendored(
                    path,
                )) => ModuleResolutionPathBufInner::StandardLibrary(AnyRelativePathBuf::Vendored(
                    path.to_path_buf(),
                )),
                ModuleResolutionPathRefInner::SitePackages(path) => {
                    ModuleResolutionPathBufInner::SitePackages(path.to_path_buf())
                }
                ModuleResolutionPathRefInner::EditableInstall(path) => {
                    ModuleResolutionPathBufInner::EditableInstall(path.to_path_buf())
                }
            };
            ModuleResolutionPathBuf(inner)
        }
    }

    #[test]
    fn path_buf_debug_impl() {
        assert_debug_snapshot!(
            ModuleResolutionPathBuf::vendored_stdlib().join("foo/bar.pyi"),
            @r###"
        ModuleResolutionPathBuf::StandardLibrary(
            AnyRelativePathBuf::Vendored(
                VendoredPathBuf(
                    "stdlib/foo/bar.pyi",
                ),
            ),
        )
        "###
        );
    }

    #[test]
    fn path_ref_debug_impl() {
        let TestCase { db, src, .. } = TestCaseBuilder::new().build();
        let first_party_path = ModuleResolutionPathBuf::extra_search_path(db.system(), src)
            .unwrap()
            .join("bar.py");
        assert_debug_snapshot!(
            first_party_path,
            @r###"
        ModuleResolutionPathBuf::Extra(
            "/src/bar.py",
        )
        "###
        );
    }

    #[test]
    fn with_extension_methods() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            stdlib_files: &[("foo.pyi", "")],
            versions: "foo: 3.8-",
        };

        let TestCase {
            db, stdlib, src, ..
        } = TestCaseBuilder::new()
            .with_custom_typeshed(TYPESHED)
            .with_target_version(TargetVersion::Py38)
            .build();

        let foo_stdlib_path = ModuleResolutionPathBuf::custom_stdlib_search_path(
            &db,
            stdlib.parent().unwrap().to_path_buf(),
        )
        .unwrap()
        .join("foo");

        assert_eq!(foo_stdlib_path.with_py_extension(), None);

        assert!(matches!(
            foo_stdlib_path.with_pyi_extension(),
            ModuleResolutionPathRef(ModuleResolutionPathRefInner::StandardLibrary(_))
        ));

        let src = ModuleResolutionPathBuf::first_party_search_path(db.system(), src).unwrap();
        assert!(matches!(
            src.join("foo/bar").with_py_extension(),
            Some(ModuleResolutionPathRef(
                ModuleResolutionPathRefInner::FirstParty(_)
            ))
        ));
    }

    #[test]
    fn module_name_1_part() {
        let TestCase { db, src, .. } = TestCaseBuilder::new().build();

        let src = ModuleResolutionPathBuf::first_party_search_path(db.system(), src).unwrap();

        assert_eq!(
            src.join("foo").to_module_name().unwrap(),
            ModuleName::new_static("foo").unwrap()
        );
        assert_eq!(
            src.join("foo.pyi").to_module_name().unwrap(),
            ModuleName::new_static("foo").unwrap()
        );
        assert_eq!(
            src.join("foo/__init__.py").to_module_name().unwrap(),
            ModuleName::new_static("foo").unwrap()
        );
    }

    #[test]
    fn module_name_2_parts() {
        let TestCase { db, src, .. } = TestCaseBuilder::new().build();

        let src = ModuleResolutionPathBuf::first_party_search_path(db.system(), src).unwrap();

        assert_eq!(
            src.join("foo/bar").to_module_name().unwrap(),
            ModuleName::new_static("foo.bar").unwrap()
        );
        assert_eq!(
            src.join("foo/bar.pyi").to_module_name().unwrap(),
            ModuleName::new_static("foo.bar").unwrap()
        );
        assert_eq!(
            src.join("foo/bar/__init__.py").to_module_name().unwrap(),
            ModuleName::new_static("foo.bar").unwrap()
        );
    }

    #[test]
    fn module_name_3_parts() {
        let TestCase { db, src, .. } = TestCaseBuilder::new().build();

        let src = ModuleResolutionPathBuf::first_party_search_path(db.system(), src).unwrap();

        assert_eq!(
            src.join("foo/bar/baz").to_module_name().unwrap(),
            ModuleName::new_static("foo.bar.baz").unwrap()
        );
        assert_eq!(
            src.join("foo/bar/baz.pyi").to_module_name().unwrap(),
            ModuleName::new_static("foo.bar.baz").unwrap()
        );
    }

    #[test]
    #[should_panic(expected = "Extension must be `pyi`; got `py`")]
    fn stdlib_path_invalid_join_py() {
        let TestCase { db, stdlib, .. } = TestCaseBuilder::new()
            .with_custom_typeshed(MockedTypeshed::default())
            .build();
        let mut stdlib = ModuleResolutionPathBuf::custom_stdlib_search_path(
            &db,
            stdlib.parent().unwrap().to_path_buf(),
        )
        .unwrap();
        stdlib.push("bar.py");
    }

    #[test]
    #[should_panic(expected = "Extension must be `py` or `pyi`; got `rs`")]
    fn non_stdlib_path_invalid_join_rs() {
        let TestCase {
            db, site_packages, ..
        } = TestCaseBuilder::new().build();

        ModuleResolutionPathBuf::site_packages_search_path(db.system(), site_packages)
            .unwrap()
            .push("bar.rs");
    }

    #[test]
    #[should_panic(expected = "already has an extension")]
    fn non_stdlib_path_too_many_extensions() {
        let TestCase {
            db, site_packages, ..
        } = TestCaseBuilder::new().build();

        ModuleResolutionPathBuf::site_packages_search_path(db.system(), site_packages)
            .unwrap()
            .join("foo.py")
            .push("foo.py");
    }

    #[test]
    fn relativize_stdlib_path_errors() {
        let TestCase { db, stdlib, .. } = TestCaseBuilder::new()
            .with_custom_typeshed(MockedTypeshed::default())
            .build();

        let root = ModuleResolutionPathBuf::custom_stdlib_search_path(
            &db,
            stdlib.parent().unwrap().to_path_buf(),
        )
        .unwrap();

        // Must have a `.pyi` extension or no extension:
        let bad_absolute_path = FilePath::System(stdlib.join("x.py"));
        assert_eq!(root.relativize_path(&bad_absolute_path), None);
        let second_bad_absolute_path = FilePath::System(stdlib.join("x.rs"));
        assert_eq!(root.relativize_path(&second_bad_absolute_path), None);

        // Must be a path that is a child of `root`:
        let third_bad_absolute_path = FilePath::system("bar/stdlib/x.pyi");
        assert_eq!(root.relativize_path(&third_bad_absolute_path), None);
    }

    #[test]
    fn relativize_non_stdlib_path_errors() {
        let TestCase { db, src, .. } = TestCaseBuilder::new().build();

        let root = ModuleResolutionPathBuf::first_party_search_path(db.system(), src).unwrap();
        // Must have a `.py` extension, a `.pyi` extension, or no extension:
        let bad_absolute_path = FilePath::system("foo/stdlib/x.rs");
        assert_eq!(root.relativize_path(&bad_absolute_path), None);
        // Must be a path that is a child of `root`:
        let second_bad_absolute_path = FilePath::system("bar/stdlib/x.pyi");
        assert_eq!(root.relativize_path(&second_bad_absolute_path), None);
    }

    fn typeshed_test_case(
        typeshed: MockedTypeshed,
        target_version: TargetVersion,
    ) -> (TestDb, ModuleResolutionPathBuf) {
        let TestCase { db, stdlib, .. } = TestCaseBuilder::new()
            .with_custom_typeshed(typeshed)
            .with_target_version(target_version)
            .build();
        let stdlib = ModuleResolutionPathBuf::custom_stdlib_search_path(
            &db,
            stdlib.parent().unwrap().to_path_buf(),
        )
        .unwrap();
        (db, stdlib)
    }

    fn py38_typeshed_test_case(typeshed: MockedTypeshed) -> (TestDb, ModuleResolutionPathBuf) {
        typeshed_test_case(typeshed, TargetVersion::Py38)
    }

    fn py39_typeshed_test_case(typeshed: MockedTypeshed) -> (TestDb, ModuleResolutionPathBuf) {
        typeshed_test_case(typeshed, TargetVersion::Py39)
    }

    #[test]
    fn mocked_typeshed_existing_regular_stdlib_pkg_py38() {
        const VERSIONS: &str = "\
            asyncio: 3.8-
            asyncio.tasks: 3.9-3.11
        ";

        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: VERSIONS,
            stdlib_files: &[("asyncio/__init__.pyi", ""), ("asyncio/tasks.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let asyncio_regular_package = stdlib_path.join("asyncio");
        assert!(asyncio_regular_package.is_directory(&resolver));
        assert!(asyncio_regular_package.is_regular_package(&resolver));
        // Paths to directories don't resolve to VfsFiles
        assert_eq!(asyncio_regular_package.to_file(&resolver), None);
        assert!(asyncio_regular_package
            .join("__init__.pyi")
            .to_file(&resolver)
            .is_some());

        // The `asyncio` package exists on Python 3.8, but the `asyncio.tasks` submodule does not,
        // according to the `VERSIONS` file in our typeshed mock:
        let asyncio_tasks_module = stdlib_path.join("asyncio/tasks.pyi");
        assert_eq!(asyncio_tasks_module.to_file(&resolver), None);
        assert!(!asyncio_tasks_module.is_directory(&resolver));
        assert!(!asyncio_tasks_module.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_existing_namespace_stdlib_pkg_py38() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "xml: 3.8-3.8",
            stdlib_files: &[("xml/etree.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let xml_namespace_package = stdlib_path.join("xml");
        assert!(xml_namespace_package.is_directory(&resolver));
        // Paths to directories don't resolve to VfsFiles
        assert_eq!(xml_namespace_package.to_file(&resolver), None);
        assert!(!xml_namespace_package.is_regular_package(&resolver));

        let xml_etree = stdlib_path.join("xml/etree.pyi");
        assert!(!xml_etree.is_directory(&resolver));
        assert!(xml_etree.to_file(&resolver).is_some());
        assert!(!xml_etree.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_single_file_stdlib_module_py38() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "functools: 3.8-",
            stdlib_files: &[("functools.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let functools_module = stdlib_path.join("functools.pyi");
        assert!(functools_module.to_file(&resolver).is_some());
        assert!(!functools_module.is_directory(&resolver));
        assert!(!functools_module.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_nonexistent_regular_stdlib_pkg_py38() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "collections: 3.9-",
            stdlib_files: &[("collections/__init__.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let collections_regular_package = stdlib_path.join("collections");
        assert_eq!(collections_regular_package.to_file(&resolver), None);
        assert!(!collections_regular_package.is_directory(&resolver));
        assert!(!collections_regular_package.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_nonexistent_namespace_stdlib_pkg_py38() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "importlib: 3.9-",
            stdlib_files: &[("importlib/abc.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let importlib_namespace_package = stdlib_path.join("importlib");
        assert_eq!(importlib_namespace_package.to_file(&resolver), None);
        assert!(!importlib_namespace_package.is_directory(&resolver));
        assert!(!importlib_namespace_package.is_regular_package(&resolver));

        let importlib_abc = stdlib_path.join("importlib/abc.pyi");
        assert_eq!(importlib_abc.to_file(&resolver), None);
        assert!(!importlib_abc.is_directory(&resolver));
        assert!(!importlib_abc.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_nonexistent_single_file_module_py38() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "foo: 2.6-",
            stdlib_files: &[("foo.pyi", "")],
        };

        let (db, stdlib_path) = py38_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py38);

        let non_existent = stdlib_path.join("doesnt_even_exist");
        assert_eq!(non_existent.to_file(&resolver), None);
        assert!(!non_existent.is_directory(&resolver));
        assert!(!non_existent.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_existing_regular_stdlib_pkgs_py39() {
        const VERSIONS: &str = "\
            asyncio: 3.8-
            asyncio.tasks: 3.9-3.11
            collections: 3.9-
        ";

        const STDLIB: &[FileSpec] = &[
            ("asyncio/__init__.pyi", ""),
            ("asyncio/tasks.pyi", ""),
            ("collections/__init__.pyi", ""),
        ];

        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: VERSIONS,
            stdlib_files: STDLIB,
        };

        let (db, stdlib_path) = py39_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py39);

        // Since we've set the target version to Py39,
        // `collections` should now exist as a directory, according to VERSIONS...
        let collections_regular_package = stdlib_path.join("collections");
        assert!(collections_regular_package.is_directory(&resolver));
        assert!(collections_regular_package.is_regular_package(&resolver));
        // (This is still `None`, as directories don't resolve to `Vfs` files)
        assert_eq!(collections_regular_package.to_file(&resolver), None);
        assert!(collections_regular_package
            .join("__init__.pyi")
            .to_file(&resolver)
            .is_some());

        // ...and so should the `asyncio.tasks` submodule (though it's still not a directory):
        let asyncio_tasks_module = stdlib_path.join("asyncio/tasks.pyi");
        assert!(asyncio_tasks_module.to_file(&resolver).is_some());
        assert!(!asyncio_tasks_module.is_directory(&resolver));
        assert!(!asyncio_tasks_module.is_regular_package(&resolver));
    }

    #[test]
    fn mocked_typeshed_existing_namespace_stdlib_pkg_py39() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "importlib: 3.9-",
            stdlib_files: &[("importlib/abc.pyi", "")],
        };

        let (db, stdlib_path) = py39_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py39);

        // The `importlib` directory now also exists
        let importlib_namespace_package = stdlib_path.join("importlib");
        assert!(importlib_namespace_package.is_directory(&resolver));
        assert!(!importlib_namespace_package.is_regular_package(&resolver));
        // (This is still `None`, as directories don't resolve to `Vfs` files)
        assert_eq!(importlib_namespace_package.to_file(&resolver), None);

        // Submodules in the `importlib` namespace package also now exist:
        let importlib_abc = importlib_namespace_package.join("abc.pyi");
        assert!(!importlib_abc.is_directory(&resolver));
        assert!(!importlib_abc.is_regular_package(&resolver));
        assert!(importlib_abc.to_file(&resolver).is_some());
    }

    #[test]
    fn mocked_typeshed_nonexistent_namespace_stdlib_pkg_py39() {
        const TYPESHED: MockedTypeshed = MockedTypeshed {
            versions: "xml: 3.8-3.8",
            stdlib_files: &[("xml/etree.pyi", "")],
        };

        let (db, stdlib_path) = py39_typeshed_test_case(TYPESHED);
        let resolver = ResolverState::new(&db, TargetVersion::Py39);

        // The `xml` package no longer exists on py39:
        let xml_namespace_package = stdlib_path.join("xml");
        assert_eq!(xml_namespace_package.to_file(&resolver), None);
        assert!(!xml_namespace_package.is_directory(&resolver));
        assert!(!xml_namespace_package.is_regular_package(&resolver));

        let xml_etree = xml_namespace_package.join("etree.pyi");
        assert_eq!(xml_etree.to_file(&resolver), None);
        assert!(!xml_etree.is_directory(&resolver));
        assert!(!xml_etree.is_regular_package(&resolver));
    }
}
