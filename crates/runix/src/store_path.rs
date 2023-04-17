use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use once_cell::sync::Lazy;
use thiserror::Error;

static STORE_PREFIX: Lazy<&str> =
    Lazy::new(|| option_env!("NIX_STORE_PATH").unwrap_or("/nix/store"));

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StorePath(PathBuf);

impl TryFrom<PathBuf> for StorePath {
    type Error = StorePathError;

    fn try_from(value: PathBuf) -> Result<Self, Self::Error> {
        let path = value
            .as_path()
            .strip_prefix(*STORE_PREFIX)
            .map_err(|_| StorePathError::NotAStorePath(value.clone()))?;

        path.components()
            .next()
            .map(|store_path| Path::new(*STORE_PREFIX).join(store_path))
            .map(StorePath)
            .ok_or(StorePathError::NotAStorePath(value))
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
        write!(f, "{}", self.0.to_string_lossy())
    }
}

impl AsRef<Path> for StorePath {
    fn as_ref(&self) -> &Path {
        self.0.as_ref()
    }
}

#[derive(Debug, Error)]
pub enum StorePathError {
    #[error("'{0}' is not a store path")]
    NotAStorePath(PathBuf),
}
