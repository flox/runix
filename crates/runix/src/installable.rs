//! A much simplified installable representation

use std::fmt::Display;
use std::str::FromStr;

use thiserror::Error;

use crate::flake_ref::{FlakeRef, ParseFlakeRefError};

/// A simplified installable representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Installable {
    pub flakeref: FlakeRef,
    pub attr_path: Vec<String>,
}

impl FromStr for Installable {
    type Err = ParseInstallableError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once('#') {
            Some((flakeref, attr_path)) => Ok(Installable {
                flakeref: flakeref.parse()?,
                attr_path: attr_path.split('.').map(String::from).collect(),
            }),
            None => Err(ParseInstallableError::MissingAttrPath),
        }
    }
}

impl Display for Installable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}", self.flakeref, self.attr_path.join("."))
    }
}

#[derive(Debug, Error)]
pub enum ParseInstallableError {
    #[error(transparent)]
    ParseFlakeRef(#[from] ParseFlakeRefError),
    #[error("Installable is missing an attribute path")]
    MissingAttrPath,
}
