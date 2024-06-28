use std::ops::Deref;
use std::path;

use camino::{Utf8Components, Utf8Path, Utf8PathBuf};

#[repr(transparent)]
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct VendoredPath(Utf8Path);

impl VendoredPath {
    #[must_use]
    pub fn new(path: &(impl AsRef<Utf8Path> + ?Sized)) -> &Self {
        let path = path.as_ref();
        // SAFETY: VendoredPath is marked as #[repr(transparent)] so the conversion from a
        // *const Utf8Path to a *const VendoredPath is valid.
        unsafe { &*(path as *const Utf8Path as *const VendoredPath) }
    }

    /// Converts the path to an owned [`VendoredPathBuf`]
    #[must_use]
    pub fn to_path_buf(&self) -> VendoredPathBuf {
        VendoredPathBuf(self.0.to_path_buf())
    }

    /// Yields the underlying [`str`] slice.
    #[must_use]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    #[must_use]
    pub fn as_std_path(&self) -> &path::Path {
        self.0.as_std_path()
    }

    pub fn components(&self) -> Utf8Components {
        self.0.components()
    }

    /// Determines whether `child` is a suffix of `self`.
    ///
    /// Only considers whole path components to match.
    #[inline]
    #[must_use]
    pub fn ends_with(&self, child: impl AsRef<VendoredPath>) -> bool {
        self.0.ends_with(child.as_ref())
    }

    /// Returns the `VendoredPath` without its final component, if there is one.
    ///
    /// Returns [`None`] if the path terminates in a root or prefix.
    #[must_use]
    pub fn parent(&self) -> Option<&VendoredPath> {
        self.0.parent().map(VendoredPath::new)
    }

    /// Returns the final component of the `VendoredPath`, if there is one.
    ///
    /// If the path is a normal file, this is the file name. If it's the path of a directory, this
    /// is the directory name.
    ///
    /// Returns [`None`] if the path terminates in `..`.
    #[inline]
    #[must_use]
    pub fn file_name(&self) -> Option<&str> {
        self.0.file_name()
    }

    /// Extracts the stem (non-extension) portion of [`self.file_name`].
    ///
    /// [`self.file_name`]: VendoredPath::file_name
    ///
    /// The stem is:
    ///
    /// * [`None`], if there is no file name;
    /// * The entire file name if there is no embedded `.`;
    /// * The entire file name if the file name begins with `.` and has no other `.`s within;
    /// * Otherwise, the portion of the file name before the final `.`
    #[must_use]
    pub fn file_stem(&self) -> Option<&str> {
        self.0.file_stem()
    }

    /// Creates an owned [`VendoredPathBuf`] with `path` adjoined to `self`.
    ///
    /// See [`std::path::PathBuf::push`] for more details on what it means to adjoin a path.
    #[inline]
    #[must_use]
    pub fn join(&self, path: impl AsRef<VendoredPath>) -> VendoredPathBuf {
        Self::new(&self.0.join(&path.as_ref().0)).to_path_buf()
    }

    /// Creates an owned [`VendoredPathBuf`] like `self` but with the given extension.
    ///
    /// See [`std::path::PathBuf::set_extension`] for more details.
    #[inline]
    #[must_use]
    pub fn with_extension(&self, extension: &str) -> VendoredPathBuf {
        VendoredPathBuf::from_utf8_path_buf(self.0.with_extension(extension))
    }

    /// Returns a path that, when joined onto `base`, yields `self`.
    ///
    /// # Errors
    ///
    /// If `base` is not a prefix of `self` (i.e., [`starts_with`]
    /// returns `false`), returns [`Err`].
    ///
    /// [`starts_with`]: FileSystemPath::starts_with
    #[inline]
    pub fn strip_prefix(
        &self,
        base: impl AsRef<VendoredPath>,
    ) -> std::result::Result<&VendoredPath, path::StripPrefixError> {
        self.0.strip_prefix(base.as_ref()).map(VendoredPath::new)
    }
}

#[repr(transparent)]
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct VendoredPathBuf(Utf8PathBuf);

impl Default for VendoredPathBuf {
    fn default() -> Self {
        Self::new()
    }
}

impl VendoredPathBuf {
    #[must_use]
    pub fn new() -> Self {
        Self(Utf8PathBuf::new())
    }

    #[must_use]
    #[inline]
    pub fn as_path(&self) -> &VendoredPath {
        VendoredPath::new(&self.0)
    }

    /// Extends `self` with `path`.
    pub fn push(&mut self, part: impl AsRef<VendoredPath>) {
        self.0.push(&part.as_ref().0)
    }

    #[must_use]
    pub fn from_utf8_path_buf(path: Utf8PathBuf) -> Self {
        Self(path)
    }
}

impl AsRef<VendoredPath> for VendoredPathBuf {
    fn as_ref(&self) -> &VendoredPath {
        self.as_path()
    }
}

impl AsRef<VendoredPath> for VendoredPath {
    #[inline]
    fn as_ref(&self) -> &VendoredPath {
        self
    }
}

impl AsRef<VendoredPath> for str {
    #[inline]
    fn as_ref(&self) -> &VendoredPath {
        VendoredPath::new(self)
    }
}

impl AsRef<VendoredPath> for String {
    #[inline]
    fn as_ref(&self) -> &VendoredPath {
        VendoredPath::new(self)
    }
}

impl AsRef<path::Path> for VendoredPath {
    #[inline]
    fn as_ref(&self) -> &path::Path {
        self.0.as_std_path()
    }
}

impl Deref for VendoredPathBuf {
    type Target = VendoredPath;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

impl<'a> TryFrom<&'a path::Path> for &'a VendoredPath {
    type Error = camino::FromPathError;

    fn try_from(value: &'a path::Path) -> Result<Self, Self::Error> {
        Ok(VendoredPath::new(<&camino::Utf8Path>::try_from(value)?))
    }
}

impl TryFrom<path::PathBuf> for VendoredPathBuf {
    type Error = camino::FromPathBufError;

    fn try_from(value: path::PathBuf) -> Result<Self, Self::Error> {
        Ok(VendoredPathBuf(camino::Utf8PathBuf::try_from(value)?))
    }
}
