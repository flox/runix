use std::ffi::OsStr;
use std::fmt::Display;
use std::path::{Component, Path, PathBuf};
use std::str::FromStr;

use once_cell::sync::Lazy;
use thiserror::Error;

/// Respect [NIX_STORE_DIR](https://nixos.org/manual/nix/stable/command-ref/env-common.html#env-NIX_STORE_DIR)
pub static STORE_PREFIX: Lazy<PathBuf> = Lazy::new(|| {
    Path::new(
        &std::env::var("NIX_STORE_DIR")
            .ok()
            .or_else(|| option_env!("NIX_STORE_DIR").map(String::from))
            .unwrap_or_else(|| "/nix/store".to_string()),
    )
    .to_path_buf()
});

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StorePath {
    prefix: PathBuf,
    basename: String,
    package_path: Option<PathBuf>,
}

impl StorePath {
    /// nix store prefix, checked against [STORE_PREFIX]
    ///
    /// ```
    /// # use std::path::Path;
    /// # use runix::store_path::{StorePath, STORE_PREFIX};
    ///
    /// let path = StorePath::from_path("/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10")
    ///     .unwrap();
    /// assert_eq!(path.prefix(), Path::new(STORE_PREFIX.as_str()));
    /// ```
    pub fn prefix(&self) -> &Path {
        &self.prefix
    }

    /// the package name in the store
    ///
    /// ```
    /// # use std::path::Path;
    /// # use runix::store_path::{StorePath, STORE_PREFIX};
    ///
    /// let path = StorePath::from_path("/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10")
    ///     .unwrap();
    /// assert_eq!(
    ///     path.name(),
    ///     "7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10"
    /// );
    /// ```
    pub fn basename(&self) -> &str {
        &self.basename
    }

    /// the package's path in the nix store
    ///
    /// drops all further components that might have been originally passed
    ///
    /// ```
    /// # use std::path::Path;
    /// # use runix::store_path::{StorePath, STORE_PREFIX};
    ///
    /// let out_path = "/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10";
    ///
    /// let path = StorePath::from_path(&out_path).unwrap();
    /// assert_eq!(path.out_path(), Path::new(&out_path));
    ///
    /// let inside_out_path = "/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10/bin/python";
    /// let path = StorePath::from_path(&inside_out_path).unwrap();
    /// assert_eq!(path.out_path(), Path::new(&out_path)); // !! without /bin/python
    /// ```
    pub fn out_path(&self) -> PathBuf {
        self.prefix.join(&self.basename)
    }

    /// path of a file or directory inside the package
    ///
    /// [None] if not present in passed path
    ///
    /// ```
    /// # use std::path::Path;
    /// # use runix::store_path::{StorePath, STORE_PREFIX};
    ///
    /// let out_path = "/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10";
    ///
    /// let path = StorePath::from_path(&out_path).unwrap();
    /// assert_eq!(path.package_path(), None);
    ///
    /// let inside_out_path = "/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10/bin/python";
    /// let path = StorePath::from_path(&inside_out_path).unwrap();
    /// assert_eq!(path.package_path(), Some(Path::new("bin/python")));
    /// ```
    pub fn package_path(&self) -> Option<&Path> {
        self.package_path.as_deref()
    }

    /// mutable reference to path of a file or directory inside the package
    ///
    /// see also: [`StorePath::package_path`]
    ///
    /// ```ignore
    /// /nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10/bin/python
    ///                                                            ^^^^^^^^^^^
    /// ```
    pub fn package_path_mut(&mut self) -> &mut Option<PathBuf> {
        &mut self.package_path
    }

    /// Combine components of store path into a native path type
    ///
    /// If parsed from a path, should return an equivalent path
    ///
    /// ```
    /// # use std::path::Path;
    /// # use runix::store_path::{StorePath, STORE_PREFIX};
    ///
    /// let path = Path::new("/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10/bin/python");
    /// assert_eq!(StorePath::from_path(&path).unwrap().as_path(), path);
    /// ```
    pub fn as_path(&self) -> PathBuf {
        if let Some(ref path) = self.package_path {
            self.out_path().join(path)
        } else {
            self.out_path()
        }
    }

    /// Try parsing a store path from a pathbuf
    ///
    /// Will return an error
    ///
    /// * if the path is relative and does not resolve to a store path
    /// * if the path's prefix does not equal the [STORE_PREFIX]
    /// * if the path contains '..' components
    /// * if the path only contains the [STORE_PREFIX]
    /// ```
    /// # use runix::store_path::{StorePath, StorePathError, STORE_PREFIX};
    ///
    /// assert!(matches!(
    ///     StorePath::from_path("./relative"),
    ///     Err(StorePathError::RelativePath(_, _))
    /// ));
    ///
    /// assert!(matches!(
    ///     StorePath::from_path("/var/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10"),
    ///     Err(StorePathError::NotAStorePath(_))
    /// ));
    ///
    /// assert!(matches!(
    ///     StorePath::from_path(
    ///         "/nix/store/7rjqb838snvvxcmpvck1smfxhkwzqal5-python3-3.10.10/bin/../lib"
    ///     ),
    ///     Err(StorePathError::RelativeComponent(_))
    /// ));
    ///
    /// assert!(matches!(
    ///     StorePath::from_path("/nix/store/"),
    ///     Err(StorePathError::NoPackage(_))
    /// ));
    /// ```
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, StorePathError> {
        Self::try_from(path.as_ref().to_path_buf())
    }

    /// Create a store path from its components
    ///
    /// Should be used with care.
    /// Use [`StorePath::from_path`] instead to create a validated store path
    pub fn new_unchecked(
        prefix: impl AsRef<Path>,
        name: impl AsRef<str>,
        package_path: Option<impl AsRef<Path>>,
    ) -> Self {
        StorePath {
            prefix: prefix.as_ref().to_path_buf(),
            basename: name.as_ref().to_string(),
            package_path: package_path.map(|p| p.as_ref().to_path_buf()),
        }
    }
}

impl TryFrom<PathBuf> for StorePath {
    type Error = StorePathError;

    fn try_from(mut value: PathBuf) -> Result<Self, Self::Error> {
        // canonicalize relative path to resolve paths for `./result/` links
        if value.is_relative() {
            value = value
                .canonicalize()
                .map_err(|e| StorePathError::RelativePath(value, e))?;
        }

        let mut components = value
            .as_path()
            .strip_prefix(&*STORE_PREFIX)
            .map_err(|_| StorePathError::NotAStorePath(value.clone()))?
            .components()
            .peekable();

        if !components
            .clone()
            .filter(|component| {
                component != &Component::CurDir || component != &Component::Normal(OsStr::new(""))
            })
            .all(|component| matches!(component, Component::Normal(_)))
        {
            return Err(StorePathError::RelativeComponent(value));
        }

        let basename = components
            .next()
            .ok_or_else(|| StorePathError::NoPackage(value.clone()))?
            .as_os_str()
            .to_string_lossy()
            .into_owned();

        let mut path = StorePath {
            prefix: STORE_PREFIX.to_path_buf(),
            basename,
            package_path: None,
        };

        if components.peek().is_some() {
            let _ = path.package_path_mut().insert(components.collect());
        }

        Ok(path)
    }
}

impl FromStr for StorePath {
    type Err = StorePathError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Path::new(s).to_path_buf().try_into()
    }
}

impl Display for StorePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_path().to_string_lossy())
    }
}

#[derive(Debug, Error)]
pub enum StorePathError {
    #[error("'{0}' is a relative path and could not be canonicalized")]
    RelativePath(PathBuf, std::io::Error),
    #[error("'{0}' contains invalid components (e.g. '..')")]
    RelativeComponent(PathBuf),
    #[error("'{0}' is not a store path (not a child of {:?})", &*STORE_PREFIX)]
    NotAStorePath(PathBuf),
    #[error("'{0}' is mising a package directory")]
    NoPackage(PathBuf),
}
