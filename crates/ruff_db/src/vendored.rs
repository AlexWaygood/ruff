use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::{self, Debug};
use std::io::{self, Read};
use std::sync::{Mutex, MutexGuard};

use zip::{read::ZipFile, ZipArchive};

use crate::file_revision::FileRevision;
pub use path::{VendoredPath, VendoredPathBuf};

pub mod path;

type Result<T> = io::Result<T>;
type LockedZipArchive<'a> = MutexGuard<'a, VendoredZipArchive>;

/// File system that stores all content in a static zip archive
/// bundled as part of the Ruff binary.
///
/// "Files" in the `VendoredFileSystem` are read-only and immutable.
/// Directories are supported, but symlinks and hardlinks cannot exist.
pub struct VendoredFileSystem {
    inner: Mutex<VendoredZipArchive>,
}

impl VendoredFileSystem {
    pub fn new(raw_bytes: &'static [u8]) -> Result<Self> {
        Ok(Self {
            inner: Mutex::new(VendoredZipArchive::new(raw_bytes)?),
        })
    }

    pub fn exists(&self, path: &VendoredPath) -> bool {
        let normalized = NormalizedVendoredPath::from(path);
        let mut archive = self.lock_archive();

        // Must probe the zipfile twice, as "stdlib" and "stdlib/" are considered
        // different paths in a zip file, but we want to abstract over that difference here
        // so that paths relative to the `VendoredFileSystem`
        // work the same as other paths in Ruff.
        archive.lookup_path(&normalized).is_ok()
            || archive
                .lookup_path(&normalized.with_trailing_slash())
                .is_ok()
    }

    pub fn metadata(&self, path: &VendoredPath) -> Option<Metadata> {
        let normalized = NormalizedVendoredPath::from(path);
        let mut archive = self.lock_archive();

        // Must probe the zipfile twice, as "stdlib" and "stdlib/" are considered
        // different paths in a zip file, but we want to abstract over that difference here
        // so that paths relative to the `VendoredFileSystem`
        // work the same as other paths in Ruff.
        if let Ok(zip_file) = archive.lookup_path(&normalized) {
            return Some(Metadata::from_zip_file(zip_file));
        }
        if let Ok(zip_file) = archive.lookup_path(&normalized.with_trailing_slash()) {
            return Some(Metadata::from_zip_file(zip_file));
        }

        None
    }

    pub fn is_directory(&self, path: &VendoredPath) -> bool {
        let normalized = NormalizedVendoredPath::from(path);
        let mut archive = self.lock_archive();
        archive
            .lookup_path(&normalized)
            .is_ok_and(|zip_file| zip_file.is_dir())
            || archive
                .lookup_path(&normalized.with_trailing_slash())
                .is_ok_and(|zip_file| zip_file.is_dir())
    }

    /// Read the entire contents of the zip file at `path` into a string
    ///
    /// Returns an Err() if any of the following are true:
    /// - The path does not exist in the underlying zip archive
    /// - The path exists in the underlying zip archive, but represents a directory
    /// - The contents of the zip file at `path` contain invalid UTF-8
    pub fn read(&self, path: &VendoredPath) -> Result<String> {
        let mut archive = self.lock_archive();
        let mut zip_file = archive.lookup_path(&NormalizedVendoredPath::from(path))?;
        let mut buffer = String::new();
        zip_file.read_to_string(&mut buffer)?;
        Ok(buffer)
    }

    /// Acquire a lock on the underlying zip archive.
    /// The call will block until it is able to acquire the lock.
    ///
    /// ## Panics:
    /// If the current thread already holds the lock.
    fn lock_archive(&self) -> LockedZipArchive {
        self.inner.lock().unwrap()
    }
}

impl fmt::Debug for VendoredFileSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut archive = self.lock_archive();
        if f.alternate() {
            let mut paths: Vec<String> = archive.0.file_names().map(String::from).collect();
            paths.sort();
            let debug_info: BTreeMap<String, ZipFileDebugInfo> = paths
                .iter()
                .map(|path| {
                    (
                        path.to_owned(),
                        ZipFileDebugInfo::from(archive.0.by_name(path).unwrap()),
                    )
                })
                .collect();
            f.debug_struct("VendoredFileSystem")
                .field("inner_mutex_poisoned", &self.inner.is_poisoned())
                .field("paths", &paths)
                .field("data_by_path", &debug_info)
                .finish()
        } else {
            write!(f, "VendoredFileSystem(<{} paths>)", archive.len())
        }
    }
}

/// Private struct only used in `Debug` implementations
///
/// This could possibly be unified with the `Metadata` struct,
/// but that is deliberately kept small, and only exposes metadata
/// that users of the `VendoredFileSystem` could realistically need.
/// For debugging purposes, however, we want to have all information
/// available.
#[allow(unused)]
#[derive(Debug)]
struct ZipFileDebugInfo {
    crc32_hash: u32,
    compressed_size: u64,
    uncompressed_size: u64,
    compression_method: zip::CompressionMethod,
    kind: FileType,
}

impl<'a> From<ZipFile<'a>> for ZipFileDebugInfo {
    fn from(value: ZipFile<'a>) -> Self {
        Self {
            crc32_hash: value.crc32(),
            compressed_size: value.compressed_size(),
            uncompressed_size: value.size(),
            compression_method: value.compression(),
            kind: if value.is_dir() {
                FileType::Directory
            } else {
                FileType::File
            },
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FileType {
    /// The path exists in the zip archive and represents a vendored file
    File,

    /// The path exists in the zip archive and represents a vendored directory of files
    Directory,
}

impl FileType {
    pub const fn is_file(self) -> bool {
        matches!(self, Self::File)
    }

    pub const fn is_directory(self) -> bool {
        matches!(self, Self::Directory)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Metadata {
    kind: FileType,
    revision: FileRevision,
}

impl Metadata {
    fn from_zip_file(zip_file: ZipFile) -> Self {
        let kind = if zip_file.is_dir() {
            FileType::Directory
        } else {
            FileType::File
        };

        Self {
            kind,
            revision: FileRevision::new(u128::from(zip_file.crc32())),
        }
    }

    pub fn kind(&self) -> FileType {
        self.kind
    }

    pub fn revision(&self) -> FileRevision {
        self.revision
    }
}

/// Newtype wrapper around a ZipArchive.
#[derive(Debug)]
struct VendoredZipArchive(ZipArchive<io::Cursor<&'static [u8]>>);

impl VendoredZipArchive {
    fn new(data: &'static [u8]) -> Result<Self> {
        Ok(Self(ZipArchive::new(io::Cursor::new(data))?))
    }

    fn lookup_path(&mut self, path: &NormalizedVendoredPath) -> Result<ZipFile> {
        Ok(self.0.by_name(path.as_str())?)
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

/// A path that has been normalized via the `normalize_vendored_path` function.
///
/// Trailing slashes are normalized away by `camino::Utf8PathBuf`s,
/// but trailing slashes are crucial for distinguishing between
/// files and directories inside zip archives.
#[derive(Debug, Clone, PartialEq, Eq)]
struct NormalizedVendoredPath<'a>(Cow<'a, str>);

impl<'a> NormalizedVendoredPath<'a> {
    fn with_trailing_slash(self) -> Self {
        debug_assert!(!self.0.ends_with('/'));
        let mut data = self.0.into_owned();
        data.push('/');
        Self(Cow::Owned(data))
    }

    fn as_str(&self) -> &str {
        &self.0
    }
}

impl<'a> From<&'a VendoredPath> for NormalizedVendoredPath<'a> {
    /// Normalize the path.
    ///
    /// The normalizations are:
    /// - Remove `.` and `..` components
    /// - Strip trailing slashes
    /// - Normalize `\\` separators to `/`
    /// - Validate that the path does not have any unsupported components
    ///
    /// ## Panics:
    /// If a path with an unsupported component for vendored paths is passed.
    /// Unsupported components are path prefixes and path root directories.
    fn from(path: &'a VendoredPath) -> Self {
        /// Remove `.` and `..` components, and validate that unsupported components are not present.
        ///
        /// This inner routine also strips trailing slashes,
        /// and normalizes paths to use Unix `/` separators.
        /// However, it always allocates, so avoid calling it if possible.
        /// In most cases, the path should already be normalized.
        fn normalize_unnormalized_path(path: &VendoredPath) -> String {
            let mut normalized_parts = Vec::new();
            for component in path.components() {
                match component {
                    camino::Utf8Component::Normal(part) => normalized_parts.push(part),
                    camino::Utf8Component::CurDir => continue,
                    camino::Utf8Component::ParentDir => {
                        // `VendoredPath("")`, `VendoredPath("..")` and `VendoredPath("../..")`
                        // all resolve to the same path relative to the zip archive
                        // (see https://github.com/astral-sh/ruff/pull/11991#issuecomment-2185278014)
                        normalized_parts.pop();
                    }
                    unsupported => {
                        panic!("Unsupported component in a vendored path: {unsupported}")
                    }
                }
            }
            normalized_parts.join("/")
        }

        let path_str = path.as_str();

        if std::path::MAIN_SEPARATOR == '\\' && path_str.contains('\\') {
            // Normalize paths so that they always use Unix path separators
            NormalizedVendoredPath(Cow::Owned(normalize_unnormalized_path(path)))
        } else if !path
            .components()
            .all(|component| matches!(component, camino::Utf8Component::Normal(_)))
        {
            // Remove non-`Normal` components
            NormalizedVendoredPath(Cow::Owned(normalize_unnormalized_path(path)))
        } else {
            // Strip trailing slashes from the path
            NormalizedVendoredPath(Cow::Borrowed(path_str.trim_end_matches('/')))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use insta::assert_snapshot;
    use once_cell::sync::Lazy;
    use zip::write::FileOptions;
    use zip::{CompressionMethod, ZipWriter};

    use super::*;

    const FUNCTOOLS_CONTENTS: &str = "def update_wrapper(): ...";
    const ASYNCIO_TASKS_CONTENTS: &str = "class Task: ...";

    static MOCK_ZIP_ARCHIVE: Lazy<Box<[u8]>> = Lazy::new(|| {
        let mut typeshed_buffer = Vec::new();
        let typeshed = io::Cursor::new(&mut typeshed_buffer);

        let options = FileOptions::default()
            .compression_method(CompressionMethod::Zstd)
            .unix_permissions(0o644);

        {
            let mut archive = ZipWriter::new(typeshed);

            archive.add_directory("stdlib/", options).unwrap();
            archive.start_file("stdlib/functools.pyi", options).unwrap();
            archive.write_all(FUNCTOOLS_CONTENTS.as_bytes()).unwrap();

            archive.add_directory("stdlib/asyncio/", options).unwrap();
            archive
                .start_file("stdlib/asyncio/tasks.pyi", options)
                .unwrap();
            archive
                .write_all(ASYNCIO_TASKS_CONTENTS.as_bytes())
                .unwrap();

            archive.finish().unwrap();
        }

        typeshed_buffer.into_boxed_slice()
    });

    fn mock_typeshed() -> VendoredFileSystem {
        VendoredFileSystem::new(&MOCK_ZIP_ARCHIVE).unwrap()
    }

    #[test]
    fn filesystem_debug_implementation() {
        assert_snapshot!(
            format!("{:?}", mock_typeshed()),
            @"VendoredFileSystem(<4 paths>)"
        );
    }

    #[test]
    fn filesystem_debug_implementation_alternate() {
        assert_snapshot!(format!("{:#?}", mock_typeshed()), @r###"
        VendoredFileSystem {
            inner_mutex_poisoned: false,
            paths: [
                "stdlib/",
                "stdlib/asyncio/",
                "stdlib/asyncio/tasks.pyi",
                "stdlib/functools.pyi",
            ],
            data_by_path: {
                "stdlib/": ZipFileDebugInfo {
                    crc32_hash: 0,
                    compressed_size: 0,
                    uncompressed_size: 0,
                    compression_method: Stored,
                    kind: Directory,
                },
                "stdlib/asyncio/": ZipFileDebugInfo {
                    crc32_hash: 0,
                    compressed_size: 0,
                    uncompressed_size: 0,
                    compression_method: Stored,
                    kind: Directory,
                },
                "stdlib/asyncio/tasks.pyi": ZipFileDebugInfo {
                    crc32_hash: 2826547428,
                    compressed_size: 24,
                    uncompressed_size: 15,
                    compression_method: Zstd,
                    kind: File,
                },
                "stdlib/functools.pyi": ZipFileDebugInfo {
                    crc32_hash: 1099005079,
                    compressed_size: 34,
                    uncompressed_size: 25,
                    compression_method: Zstd,
                    kind: File,
                },
            },
        }
        "###);
    }

    fn test_directory(dirname: &str) {
        let mock_typeshed = mock_typeshed();

        let path = VendoredPath::new(dirname);

        assert!(mock_typeshed.exists(path));
        assert!(mock_typeshed.read(path).is_err());
        let metadata = mock_typeshed.metadata(path).unwrap();
        assert!(metadata.kind.is_directory());
    }

    #[test]
    fn stdlib_dir_no_trailing_slash() {
        test_directory("stdlib")
    }

    #[test]
    fn stdlib_dir_trailing_slash() {
        test_directory("stdlib/")
    }

    #[test]
    fn asyncio_dir_no_trailing_slash() {
        test_directory("stdlib/asyncio")
    }

    #[test]
    fn asyncio_dir_trailing_slash() {
        test_directory("stdlib/asyncio/")
    }

    #[test]
    fn stdlib_dir_parent_components() {
        test_directory("stdlib/asyncio/../../stdlib")
    }

    #[test]
    fn asyncio_dir_odd_components() {
        test_directory("./stdlib/asyncio/../asyncio/")
    }

    fn test_nonexistent_path(path: &str) {
        let mock_typeshed = mock_typeshed();
        let path = VendoredPath::new(path);
        assert!(!mock_typeshed.exists(path));
        assert!(mock_typeshed.metadata(path).is_none());
        assert!(mock_typeshed
            .read(path)
            .is_err_and(|err| err.to_string().contains("file not found")));
    }

    #[test]
    fn simple_nonexistent_path() {
        test_nonexistent_path("foo")
    }

    #[test]
    fn nonexistent_path_with_extension() {
        test_nonexistent_path("foo.pyi")
    }

    #[test]
    fn nonexistent_path_with_trailing_slash() {
        test_nonexistent_path("foo/")
    }

    #[test]
    fn nonexistent_path_with_fancy_components() {
        test_nonexistent_path("./foo/../../../foo")
    }

    fn test_file(mock_typeshed: &VendoredFileSystem, path: &VendoredPath) {
        assert!(mock_typeshed.exists(path));
        let metadata = mock_typeshed.metadata(path).unwrap();
        assert!(metadata.kind.is_file());
    }

    #[test]
    fn functools_file_contents() {
        let mock_typeshed = mock_typeshed();
        let path = VendoredPath::new("stdlib/functools.pyi");
        test_file(&mock_typeshed, path);
        let functools_stub = mock_typeshed.read(path).unwrap();
        assert_eq!(functools_stub.as_str(), FUNCTOOLS_CONTENTS);
        // Test that using the RefCell doesn't mutate
        // the internal state of the underlying zip archive incorrectly:
        let functools_stub_again = mock_typeshed.read(path).unwrap();
        assert_eq!(functools_stub_again.as_str(), FUNCTOOLS_CONTENTS);
    }

    #[test]
    fn functools_file_other_path() {
        test_file(
            &mock_typeshed(),
            VendoredPath::new("stdlib/../stdlib/../stdlib/functools.pyi"),
        )
    }

    #[test]
    fn asyncio_file_contents() {
        let mock_typeshed = mock_typeshed();
        let path = VendoredPath::new("stdlib/asyncio/tasks.pyi");
        test_file(&mock_typeshed, path);
        let asyncio_stub = mock_typeshed.read(path).unwrap();
        assert_eq!(asyncio_stub.as_str(), ASYNCIO_TASKS_CONTENTS);
    }

    #[test]
    fn asyncio_file_other_path() {
        test_file(
            &mock_typeshed(),
            VendoredPath::new("./stdlib/asyncio/../asyncio/tasks.pyi"),
        )
    }
}
