use crate::db::tests::{TestDb, TestDbBuilder};
use std::sync::{Arc, Mutex, OnceLock};

static CACHED_DB: OnceLock<Arc<Mutex<TestDb>>> = OnceLock::new();

/// The path to the module containing `NewType` definitions for property testing.
pub(crate) const NEWTYPE_MODULE_PATH: &str = "/src/newtypes_for_testing.py";

pub(crate) fn get_cached_db() -> TestDb {
    let db = CACHED_DB.get_or_init(|| {
        let db = TestDbBuilder::new()
            .with_file(
                NEWTYPE_MODULE_PATH,
                "from typing import NewType\n\
                 NewTypeOfInt = NewType(\"NewTypeOfInt\", int)\n\
                 NewTypeOfStr = NewType(\"NewTypeOfStr\", str)\n",
            )
            .build()
            .expect("valid TestDb setup");
        Arc::new(Mutex::new(db))
    });
    db.lock().unwrap().clone()
}
