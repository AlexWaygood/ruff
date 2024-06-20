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
    use super::*;
    use std::io::{self, Read};
    use std::path::Path;

    #[test]
    fn typeshed_zip_created_at_build_time() {
        let mut typeshed_zip_archive =
            zip::ZipArchive::new(io::Cursor::new(TYPESHED_ZIP_BYTES)).unwrap();

        let path_to_functools = Path::new("stdlib").join("functools.pyi");
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
    fn typeshed_vfs_consistent_with_typeshed_versions() {
        const VENDORED_STDLIB_DIR: &str = "../vendor/typeshed/stdlib";
        for entry in std::fs::read_dir(VENDORED_STDLIB_DIR) {
            let entry = entry.un
        }
    }
}
