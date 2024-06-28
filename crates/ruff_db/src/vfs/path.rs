use camino::Utf8Components;

use crate::file_system::{FileSystemPath, FileSystemPathBuf};
use crate::vendored::path::{VendoredPath, VendoredPathBuf};

/// Path to a file.
///
/// The path abstracts that files in Ruff can come from different sources:
///
/// * a file stored on disk
/// * a vendored file that ships as part of the ruff binary
/// * Future: A virtual file that references a slice of another file. For example, the CSS code in a python file.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum VfsPath {
    /// Path that points to a file on disk.
    FileSystem(FileSystemPathBuf),
    Vendored(VendoredPathBuf),
}

impl VfsPath {
    /// Create a new path to a file/directory on the file system.
    #[must_use]
    pub fn file_system(path: impl AsRef<FileSystemPath>) -> Self {
        VfsPath::FileSystem(path.as_ref().to_path_buf())
    }

    /// Create a new path to a file/directory in a vendored zip file
    #[must_use]
    pub fn vendored(path: impl AsRef<VendoredPath>) -> Self {
        VfsPath::Vendored(path.as_ref().to_path_buf())
    }

    /// Returns `Some` if the path is a file system path that points to a path on disk.
    #[must_use]
    #[inline]
    pub fn into_file_system_path_buf(self) -> Option<FileSystemPathBuf> {
        match self {
            VfsPath::FileSystem(path) => Some(path),
            VfsPath::Vendored(_) => None,
        }
    }

    #[must_use]
    #[inline]
    pub fn as_file_system_path(&self) -> Option<&FileSystemPath> {
        match self {
            VfsPath::FileSystem(path) => Some(path.as_path()),
            VfsPath::Vendored(_) => None,
        }
    }

    /// Returns `true` if the path is a file system path that points to a path on disk.
    #[must_use]
    #[inline]
    pub const fn is_file_system_path(&self) -> bool {
        matches!(self, VfsPath::FileSystem(_))
    }

    /// Returns `true` if the path is a vendored path.
    #[must_use]
    #[inline]
    pub const fn is_vendored_path(&self) -> bool {
        matches!(self, VfsPath::Vendored(_))
    }

    #[must_use]
    #[inline]
    pub fn as_vendored_path(&self) -> Option<&VendoredPath> {
        match self {
            VfsPath::Vendored(path) => Some(path.as_path()),
            VfsPath::FileSystem(_) => None,
        }
    }

    /// Yields the underlying [`str`] slice.
    #[must_use]
    pub fn as_str(&self) -> &str {
        match self {
            VfsPath::FileSystem(path) => path.as_str(),
            VfsPath::Vendored(path) => path.as_str(),
        }
    }

    pub fn push(&mut self, part: &str) {
        match self {
            VfsPath::FileSystem(ref mut path) => path.push(part),
            VfsPath::Vendored(ref mut path) => path.push(part),
        }
    }

    pub fn join(&self, part: &str) -> Self {
        match self {
            VfsPath::FileSystem(path) => VfsPath::FileSystem(path.join(part)),
            VfsPath::Vendored(path) => VfsPath::Vendored(path.join(part)),
        }
    }

    pub fn with_pyi_extension(&self) -> Self {
        match self {
            VfsPath::FileSystem(path) => VfsPath::FileSystem(path.with_extension("pyi")),
            VfsPath::Vendored(path) => VfsPath::Vendored(path.with_pyi_extension()),
        }
    }
}

impl AsRef<str> for VfsPath {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl From<FileSystemPathBuf> for VfsPath {
    fn from(value: FileSystemPathBuf) -> Self {
        Self::FileSystem(value)
    }
}

impl From<&FileSystemPath> for VfsPath {
    fn from(value: &FileSystemPath) -> Self {
        VfsPath::FileSystem(value.to_path_buf())
    }
}

impl From<VendoredPathBuf> for VfsPath {
    fn from(value: VendoredPathBuf) -> Self {
        Self::Vendored(value)
    }
}

impl From<&VendoredPath> for VfsPath {
    fn from(value: &VendoredPath) -> Self {
        Self::Vendored(value.to_path_buf())
    }
}

impl PartialEq<FileSystemPath> for VfsPath {
    #[inline]
    fn eq(&self, other: &FileSystemPath) -> bool {
        self.as_file_system_path()
            .is_some_and(|self_path| self_path == other)
    }
}

impl PartialEq<VfsPath> for FileSystemPath {
    #[inline]
    fn eq(&self, other: &VfsPath) -> bool {
        other == self
    }
}

impl PartialEq<FileSystemPathBuf> for VfsPath {
    #[inline]
    fn eq(&self, other: &FileSystemPathBuf) -> bool {
        self == other.as_path()
    }
}

impl PartialEq<VfsPath> for FileSystemPathBuf {
    fn eq(&self, other: &VfsPath) -> bool {
        other == self
    }
}

impl PartialEq<VendoredPath> for VfsPath {
    #[inline]
    fn eq(&self, other: &VendoredPath) -> bool {
        self.as_vendored_path()
            .is_some_and(|self_path| self_path == other)
    }
}

impl PartialEq<VfsPath> for VendoredPath {
    #[inline]
    fn eq(&self, other: &VfsPath) -> bool {
        other == self
    }
}

impl PartialEq<VendoredPathBuf> for VfsPath {
    #[inline]
    fn eq(&self, other: &VendoredPathBuf) -> bool {
        other.as_path() == self
    }
}

impl PartialEq<VfsPath> for VendoredPathBuf {
    #[inline]
    fn eq(&self, other: &VfsPath) -> bool {
        other == self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VfsPathRef<'a> {
    FileSystem(&'a FileSystemPath),
    Vendored(&'a VendoredPath),
}

impl<'a> VfsPathRef<'a> {
    #[must_use]
    pub fn ends_with(self, child: &str) -> bool {
        match self {
            Self::FileSystem(path) => path.ends_with(child),
            Self::Vendored(path) => path.ends_with(child),
        }
    }

    #[must_use]
    pub fn parent(self) -> Option<Self> {
        match self {
            Self::FileSystem(path) => path.parent().map(VfsPathRef::FileSystem),
            Self::Vendored(path) => path.parent().map(VfsPathRef::Vendored),
        }
    }

    #[must_use]
    pub fn as_str(self) -> &'a str {
        match self {
            Self::FileSystem(path) => path.as_str(),
            Self::Vendored(path) => path.as_str(),
        }
    }

    pub fn components(self) -> Utf8Components<'a> {
        match self {
            Self::FileSystem(path) => path.components(),
            Self::Vendored(path) => path.components(),
        }
    }

    #[must_use]
    pub fn file_stem(self) -> Option<&'a str> {
        match self {
            Self::FileSystem(path) => path.file_stem(),
            Self::Vendored(path) => path.file_stem(),
        }
    }

    #[must_use]
    pub fn to_path_buf(self) -> VfsPath {
        match self {
            Self::FileSystem(path) => VfsPath::FileSystem(path.to_path_buf()),
            Self::Vendored(path) => VfsPath::Vendored(path.to_path_buf()),
        }
    }
}
