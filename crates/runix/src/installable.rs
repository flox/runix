//! A much simplified installable representation

use derive_more::Constructor;

/// A much simplified installable representation
#[derive(Debug, Clone, Constructor)]
pub struct Installable {
    pub flakeref: FlakeRef,
    pub attr_path: String,
}

impl Installable {
    pub fn to_nix(&self) -> String {
        format!("{}#{}", self.flakeref, self.attr_path)
    }
}

impl From<String> for Installable {
    fn from(input: String) -> Self {
        let mut split = input.splitn(2, '#');

        match (split.next(), split.next()) {
            (Some(flakeref), Some(attr_path)) => Installable {
                flakeref: flakeref.to_owned(),
                attr_path: attr_path.to_owned(),
            },
            (Some(attr_path), None) => Installable {
                flakeref: ".".to_owned(),
                attr_path: attr_path.to_owned(),
            },
            _ => unreachable!(),
        }
    }
}

impl ToString for Installable {
    fn to_string(&self) -> String {
        self.to_nix()
    }
}

/// A much simplified FlakeRef representation
///
/// to be replaced by the checked implementation in [crate::flake_ref]
pub type FlakeRef = String;
