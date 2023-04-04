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
pub struct IndirectRef {
    pub id: String,

    #[serde(rename = "type")]
    _type: Tag,

    #[serde(flatten)]
    pub attributes: BTreeMap<String, String>,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone, PartialOrd, Ord, Default)]
pub enum Tag {
    #[default]
    #[serde(rename = "indirect")]
    Indirect,
}

impl IndirectRef {
    pub fn new(id: String, attributes: BTreeMap<String, String>) -> Self {
        Self {
            id,
            _type: Tag::Indirect,
            attributes,
        }
    }
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
        write!(f, "{prefix}:{id}", prefix = Self::scheme(), id = self.id)?;
        if !self.attributes.is_empty() {
            write!(
                f,
                "?{attributes}",
                attributes = serde_urlencoded::to_string(&self.attributes).unwrap_or_default()
            )?
        }

        Ok(())
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
        let _type = Tag::Indirect;
        Ok(IndirectRef {
            id,
            attributes,
            _type,
        })
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

    use serde_json::json;

    use super::*;
    use crate::flake_ref::tests::roundtrip_to;

    /// Ensure that an indirect flake ref serializes without information loss
    #[test]
    fn indirect_to_from_url() {
        let expect = IndirectRef {
            _type: Tag::Indirect,
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
            _type: Tag::Indirect,
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

    #[test]
    fn roundtrip_attributes() {
        roundtrip_to::<IndirectRef>("nixpkgs?ref=master&dir=1", "flake:nixpkgs?dir=1&ref=master");
    }

    #[test]
    fn tag_json() {
        let expected = json!({
            "type": "indirect",
            "id": "test",
        });

        let flakeref = IndirectRef {
            _type: Tag::Indirect,
            id: "test".to_string(),
            attributes: Default::default(),
        };

        assert_eq!(serde_json::to_value(&flakeref).unwrap(), expected);
        assert_eq!(
            serde_json::from_value::<IndirectRef>(expected).unwrap(),
            flakeref
        );
    }

    /// https://github.com/serde-rs/serde/issues/2423  :(
    #[test]
    #[ignore]
    fn xyz() {
        #[derive(Debug, Serialize, Deserialize, Eq, PartialEq)]
        #[serde(tag = "x", rename = "y")]
        pub struct MyStruct {
            #[serde(flatten)]
            pub attributes: BTreeMap<String, String>,
        }

        let expected = json!({ "x": "y", });
        let my_struct = MyStruct {
            attributes: Default::default(),
        };

        assert_eq!(serde_json::to_value(&my_struct).unwrap(), expected);
        assert_eq!(
            serde_json::from_value::<MyStruct>(expected).unwrap(),
            my_struct
        );
    }
}
