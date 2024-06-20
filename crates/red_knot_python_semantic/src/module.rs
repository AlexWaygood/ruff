use std::fmt::Formatter;
use std::sync::Arc;

use ruff_db::module_name::ModuleName;
use ruff_db::vfs::{VfsFile, VfsPath};

use crate::Db;

pub mod resolver;

/// Representation of a Python module.
#[derive(Clone, PartialEq, Eq)]
pub struct Module {
    inner: Arc<ModuleInner>,
}

impl Module {
    /// The absolute name of the module (e.g. `foo.bar`)
    pub fn name(&self) -> &ModuleName {
        &self.inner.name
    }

    /// The file to the source code that defines this module
    pub fn file(&self) -> VfsFile {
        self.inner.file
    }

    /// The search path from which the module was resolved.
    pub fn search_path(&self) -> &ModuleSearchPath {
        &self.inner.search_path
    }

    /// Determine whether this module is a single-file module or a package
    pub fn kind(&self) -> ModuleKind {
        self.inner.kind
    }
}

impl std::fmt::Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("name", &self.name())
            .field("kind", &self.kind())
            .field("file", &self.file())
            .field("search_path", &self.search_path())
            .finish()
    }
}

impl salsa::DebugWithDb<dyn Db> for Module {
    fn fmt(&self, f: &mut Formatter<'_>, db: &dyn Db) -> std::fmt::Result {
        f.debug_struct("Module")
            .field("name", &self.name())
            .field("kind", &self.kind())
            .field("file", &self.file().debug(db.upcast()))
            .field("search_path", &self.search_path())
            .finish()
    }
}

#[derive(PartialEq, Eq)]
struct ModuleInner {
    name: ModuleName,
    kind: ModuleKind,
    search_path: ModuleSearchPath,
    file: VfsFile,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ModuleKind {
    /// A single-file module (e.g. `foo.py` or `foo.pyi`)
    Module,

    /// A python package (`foo/__init__.py` or `foo/__init__.pyi`)
    Package,
}

/// A search path in which to search modules.
/// Corresponds to a path in [`sys.path`](https://docs.python.org/3/library/sys_path_init.html) at runtime.
///
/// Cloning a search path is cheap because it's an `Arc`.
#[derive(Clone, PartialEq, Eq)]
pub struct ModuleSearchPath {
    inner: Arc<ModuleSearchPathInner>,
}

impl ModuleSearchPath {
    pub fn new<P>(path: P, kind: ModuleSearchPathKind) -> Self
    where
        P: Into<VfsPath>,
    {
        Self {
            inner: Arc::new(ModuleSearchPathInner {
                path: path.into(),
                kind,
            }),
        }
    }

    /// Determine whether this is a first-party, third-party or standard-library search path
    pub fn kind(&self) -> ModuleSearchPathKind {
        self.inner.kind
    }

    /// Return the location of the search path on the file system
    pub fn path(&self) -> &VfsPath {
        &self.inner.path
    }
}

impl std::fmt::Debug for ModuleSearchPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleSearchPath")
            .field("path", &self.inner.path)
            .field("kind", &self.kind())
            .finish()
    }
}

#[derive(Eq, PartialEq)]
struct ModuleSearchPathInner {
    path: VfsPath,
    kind: ModuleSearchPathKind,
}

/// Enumeration of the different kinds of search paths type checkers are expected to support.
///
/// N.B. Although we don't implement `Ord` for this enum, they are ordered in terms of the
/// priority that we want to give these modules when resolving them.
/// This is roughly [the order given in the typing spec], but typeshed's stubs
/// for the standard library are moved higher up to match Python's semantics at runtime.
///
/// [the order given in the typing spec]: https://typing.readthedocs.io/en/latest/spec/distributing.html#import-resolution-ordering
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum ModuleSearchPathKind {
    /// "Extra" paths provided by the user in a config file, env var or CLI flag.
    /// E.g. mypy's `MYPYPATH` env var, or pyright's `stubPath` configuration setting
    Extra,

    /// Files in the project we're directly being invoked on
    FirstParty,

    /// The `stdlib` directory of typeshed (either vendored or custom)
    StandardLibrary,

    /// Stubs or runtime modules installed in site-packages
    SitePackagesThirdParty,

    /// Vendored third-party stubs from typeshed
    VendoredThirdParty,
}
