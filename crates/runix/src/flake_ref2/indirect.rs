use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use thiserror::Error;
use url::Url;

use super::FlakeRefSource;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/path.cc#L46>
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone, PartialOrd, Ord)]
#[serde(tag = "indirect")]
pub struct IndirectRef {
    pub id: String,
    #[serde(flatten)]
    pub attributes: BTreeMap<String, String>,
}

impl FlakeRefSource for IndirectRef {
    fn scheme() -> Cow<'static, str> {
        "flake".into()
    }

    fn parses(maybe_ref: &str) -> bool {
        if maybe_ref.starts_with("flake:") {
            return true;
        }

        if maybe_ref.contains(':') {
            return false;
        }

        ('a'..='z').any(|prefix| maybe_ref.starts_with(prefix))
            || ('A'..='Z').any(|prefix| maybe_ref.starts_with(prefix))
    }
}

impl Display for IndirectRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{prefix}:{id}{attributes}",
            prefix = Self::scheme(),
            id = self.id,
            attributes = serde_urlencoded::to_string(&self.attributes).unwrap_or_default()
        )
    }
}

impl FromStr for IndirectRef {
    type Err = ParseIndirectError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = match Url::parse(s) {
            Ok(url) if url.scheme() == Self::scheme() => url,
            Ok(url_bad_scheme) => Err(ParseIndirectError::InvalidScheme(
                url_bad_scheme.scheme().to_string(),
                Self::scheme().into_owned(),
            ))?,
            Err(_) if Self::parses(s) && !s.starts_with(&*Self::scheme()) => {
                Url::parse(&format!("{scheme}:{s}", scheme = Self::scheme()))?
            },
            e => e?,
        };

        let id = url.path().to_string();
        let attributes = serde_urlencoded::from_str(url.query().unwrap_or_default())?;
        Ok(IndirectRef { id, attributes })
    }
}

#[derive(Debug, Error)]
pub enum ParseIndirectError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("Couldn't parse query: {0}")]
    Query(#[from] serde_urlencoded::de::Error),
}

#[cfg(test)]
mod tests {

    use super::*;

    /// Ensure that an indirect flake ref serializes without information loss
    #[test]
    fn indirect_to_from_url() {
        let expect = IndirectRef {
            id: "nixpkgs-flox".into(),
            attributes: BTreeMap::default(),
        };

        let flakeref = "flake:nixpkgs-flox";

        assert_eq!(flakeref.parse::<IndirectRef>().unwrap(), expect);
        assert_eq!(expect.to_string(), flakeref);
    }

    #[test]
    fn parses_registry_flakeref() {
        let expected = IndirectRef {
            id: "nixpkgs".to_string(),
            attributes: BTreeMap::default(),
        };

        assert_eq!(IndirectRef::from_str("flake:nixpkgs").unwrap(), expected);
        assert_eq!(IndirectRef::from_str("nixpkgs").unwrap(), expected);
    }

    #[test]
    fn does_not_parse_other() {
        IndirectRef::from_str("github:nixpkgs").unwrap_err();
    }
}
