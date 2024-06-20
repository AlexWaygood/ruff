use std::fmt;
use std::ops::Deref;

use crate::file_system::FileSystemPath;

use ruff_python_stdlib::identifiers::is_identifier;

/// A module name, e.g. `foo.bar`.
///
/// Always normalized to the absolute form (never a relative module name, i.e., never `.foo`).
#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct ModuleName(smol_str::SmolStr);

impl ModuleName {
    /// Creates a new module name for `name`. Returns `Some` if `name` is a valid, absolute
    /// module name and `None` otherwise.
    ///
    /// The module name is invalid if:
    ///
    /// * The name is empty
    /// * The name is relative
    /// * The name ends with a `.`
    /// * The name contains a sequence of multiple dots
    /// * A component of a name (the part between two dots) isn't a valid python identifier.
    #[inline]
    pub fn new(name: &str) -> Option<Self> {
        Self::new_from_smol(smol_str::SmolStr::new(name))
    }

    /// Creates a new module name for `name` where `name` is a static string.
    /// Returns `Some` if `name` is a valid, absolute module name and `None` otherwise.
    ///
    /// The module name is invalid if:
    ///
    /// * The name is empty
    /// * The name is relative
    /// * The name ends with a `.`
    /// * The name contains a sequence of multiple dots
    /// * A component of a name (the part between two dots) isn't a valid python identifier.
    ///
    /// ## Examples
    ///
    /// ```
    /// use ruff_python_stdlib::module::ModuleName;
    ///
    /// assert_eq!(ModuleName::new_static("foo.bar").as_deref(), Some("foo.bar"));
    /// assert_eq!(ModuleName::new_static(""), None);
    /// assert_eq!(ModuleName::new_static("..foo"), None);
    /// assert_eq!(ModuleName::new_static(".foo"), None);
    /// assert_eq!(ModuleName::new_static("foo."), None);
    /// assert_eq!(ModuleName::new_static("foo..bar"), None);
    /// assert_eq!(ModuleName::new_static("2000"), None);
    /// ```
    #[inline]
    pub fn new_static(name: &'static str) -> Option<Self> {
        Self::new_from_smol(smol_str::SmolStr::new_static(name))
    }

    fn new_from_smol(name: smol_str::SmolStr) -> Option<Self> {
        if name.is_empty() {
            return None;
        }

        if name.split('.').all(is_identifier) {
            Some(Self(name))
        } else {
            None
        }
    }

    /// An iterator over the components of the module name:
    ///
    /// # Examples
    ///
    /// ```
    /// use ruff_python_stdlib::module::ModuleName;
    ///
    /// assert_eq!(ModuleName::new_static("foo.bar.baz").unwrap().components().collect::<Vec<_>>(), vec!["foo", "bar", "baz"]);
    /// ```
    pub fn components(&self) -> impl DoubleEndedIterator<Item = &str> {
        self.0.split('.')
    }

    /// The name of this module's immediate parent, if it has a parent.
    ///
    /// # Examples
    ///
    /// ```
    /// use ruff_python_stdlib::module::ModuleName;
    ///
    /// assert_eq!(ModuleName::new_static("foo.bar").unwrap().parent(), Some(ModuleName::new_static("foo").unwrap()));
    /// assert_eq!(ModuleName::new_static("foo.bar.baz").unwrap().parent(), Some(ModuleName::new_static("foo.bar").unwrap()));
    /// assert_eq!(ModuleName::new_static("root").unwrap().parent(), None);
    /// ```
    pub fn parent(&self) -> Option<ModuleName> {
        let (parent, _) = self.0.rsplit_once('.')?;

        Some(Self(smol_str::SmolStr::new(parent)))
    }

    /// Returns `true` if the name starts with `other`.
    ///
    /// This is equivalent to checking if `self` is a sub-module of `other`.
    ///
    /// # Examples
    ///
    /// ```
    /// use ruff_python_stdlib::module::ModuleName;
    ///
    /// assert!(ModuleName::new_static("foo.bar").unwrap().starts_with(&ModuleName::new_static("foo").unwrap()));
    ///
    /// assert!(!ModuleName::new_static("foo.bar").unwrap().starts_with(&ModuleName::new_static("bar").unwrap()));
    /// assert!(!ModuleName::new_static("foo_bar").unwrap().starts_with(&ModuleName::new_static("foo").unwrap()));
    /// ```
    pub fn starts_with(&self, other: &ModuleName) -> bool {
        let mut self_components = self.components();
        let other_components = other.components();

        for other_component in other_components {
            if self_components.next() != Some(other_component) {
                return false;
            }
        }

        true
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn from_relative_path(path: &FileSystemPath) -> Option<Self> {
        let path = if path.ends_with("__init__.py") || path.ends_with("__init__.pyi") {
            path.parent()?
        } else {
            path
        };

        let name = if let Some(parent) = path.parent() {
            let mut name = String::with_capacity(path.as_str().len());

            for component in parent.components() {
                name.push_str(component.as_os_str().to_str()?);
                name.push('.');
            }

            // SAFETY: Unwrap is safe here or `parent` would have returned `None`.
            name.push_str(path.file_stem().unwrap());

            smol_str::SmolStr::from(name)
        } else {
            smol_str::SmolStr::new(path.file_stem()?)
        };

        Some(Self(name))
    }
}

impl Deref for ModuleName {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl PartialEq<str> for ModuleName {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<ModuleName> for str {
    fn eq(&self, other: &ModuleName) -> bool {
        self == other.as_str()
    }
}

impl fmt::Display for ModuleName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}