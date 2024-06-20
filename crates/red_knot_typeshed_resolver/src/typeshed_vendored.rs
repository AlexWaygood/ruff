use std::str::FromStr;

use once_cell::sync::Lazy;

use ruff_db::vendored::VendoredFileSystem;

use crate::versions::TypeshedVersions;

// The file path here is hardcoded in this crate's `build.rs` script.
// Luckily this crate will fail to build if this file isn't available at build time.
const TYPESHED_ZIP_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/zipped_typeshed.zip"));
const VERSIONS_DATA_PATH: &str = include_str!("../vendor/typeshed/stdlib/VERSIONS");

pub(crate) static VENDORED_TYPESHED_VERSIONS: Lazy<TypeshedVersions> =
    Lazy::new(|| TypeshedVersions::from_str(VERSIONS_DATA_PATH).unwrap());
pub(crate) static VENDORED_TYPESHED_STUBS: Lazy<VendoredFileSystem> =
    Lazy::new(|| VendoredFileSystem::new(TYPESHED_ZIP_BYTES).unwrap());

#[cfg(test)]
mod tests {
    use std::io::{self, Read};
    use std::path::{Path, PathBuf};

    use once_cell::sync::Lazy;

    use ruff_db::module_name::ModuleName;
    use ruff_db::vendored::VendoredPath;

    use crate::constants;

    use super::*;

    static VENDORED_TYPESHED_DIR: Lazy<PathBuf> =
        Lazy::new(|| Path::new("vendor/typeshed").canonicalize().unwrap());

    #[test]
    fn typeshed_zip_created_at_build_time() {
        let mut typeshed_zip_archive =
            zip::ZipArchive::new(io::Cursor::new(TYPESHED_ZIP_BYTES)).unwrap();

        let path_to_functools = Path::new(constants::TYPESHED_STDLIB_DIR).join("functools.pyi");
        let mut functools_module_stub = typeshed_zip_archive
            .by_name(path_to_functools.to_str().unwrap())
            .unwrap();
        assert!(functools_module_stub.is_file());

        let mut functools_module_stub_source = String::new();
        functools_module_stub
            .read_to_string(&mut functools_module_stub_source)
            .unwrap();

        assert!(functools_module_stub_source.contains("def update_wrapper("));
    }

    #[test]
    fn typeshed_vfs_consistent_with_vendored_stubs() {
        let mut empty_iterator = true;
        for entry in walkdir::WalkDir::new(&*VENDORED_TYPESHED_DIR).min_depth(1) {
            empty_iterator = false;
            let entry = entry.unwrap();
            let absolute_path = entry.path();
            let file_type = entry.file_type();

            let relative_path = absolute_path.strip_prefix(&*VENDORED_TYPESHED_DIR).unwrap();
            let vendored_path = VendoredPath::new(relative_path.to_str().unwrap());

            assert!(VENDORED_TYPESHED_STUBS.exists(vendored_path));

            let vendored_path_kind = VENDORED_TYPESHED_STUBS
                .metadata(vendored_path)
                .unwrap()
                .kind();
            assert_eq!(vendored_path_kind.is_directory(), file_type.is_dir());
        }
        assert!(!empty_iterator);
    }

    #[test]
    fn typeshed_versions_consistent_with_vendored_stubs() {
        let mut empty_iterator = true;
        for entry in
            std::fs::read_dir(VENDORED_TYPESHED_DIR.join(constants::TYPESHED_STDLIB_DIR)).unwrap()
        {
            empty_iterator = false;
            let entry = entry.unwrap();
            let absolute_path = entry.path();
            let relative_path = absolute_path
                .strip_prefix(&*VENDORED_TYPESHED_DIR)
                .and_then(|path| path.strip_prefix(constants::TYPESHED_STDLIB_DIR))
                .unwrap();
            if relative_path == Path::new(constants::STDLIB_VERSIONS_FILE) {
                continue;
            }
            let mut top_level_module = relative_path
                .components()
                .next()
                .unwrap()
                .as_os_str()
                .to_str()
                .unwrap();
            if let Some(extension) = absolute_path.extension() {
                top_level_module = top_level_module
                    .strip_suffix(extension.to_str().unwrap())
                    .and_then(|string| string.strip_suffix('.'))
                    .unwrap();
            }
            let top_level_module = ModuleName::new(top_level_module).unwrap();
            assert!(VENDORED_TYPESHED_VERSIONS.contains_module(&top_level_module));
        }
        assert!(!empty_iterator);
    }
}
