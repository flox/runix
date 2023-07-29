use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use thiserror::Error;
use url::Url;

use super::{Attrs, FlakeRefSource};
use crate::url_parser::UrlParseError;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/path.cc#L46>
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone, PartialOrd, Ord)]
pub struct IndirectRef {
    pub id: String,

    #[serde(rename = "type")]
    pub(crate) _type: Tag,

    #[serde(flatten)]
    pub attributes: BTreeMap<String, String>,
}

impl TryFrom<Attrs> for IndirectRef {
    type Error = UrlParseError;

    fn try_from(mut attrs: Attrs) -> Result<Self, Self::Error> {
        let tag = Tag::Indirect;
        let Some(Value::String(id)) = attrs.get("id") else {
            return Err(UrlParseError::MissingAttribute("id"));
        };
        let id = id.clone();
        let mut attributes = BTreeMap::new();
        for (k, v) in attrs.drain() {
            if let Value::String(string) = v {
                // Calling Value::to_string on Value::String produces an extra
                // set of quotes around the value
                attributes.insert(k, string);
            } else {
                attributes.insert(k, v.to_string());
            }
        }
        Ok(IndirectRef {
            id,
            _type: tag,
            attributes,
        })
    }
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
    type ParseErr = ParseIndirectError;

    fn scheme() -> Cow<'static, str> {
        "flake".into()
    }

    fn from_url(url: Url) -> Result<Self, Self::ParseErr> {
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
            e => e?,
        };
        Self::from_url(url)
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
    use crate::flake_ref::FlakeRef;
    use crate::url_parser::PARSER_UTIL_BIN_PATH;

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
        let expected_attrs = vec![
            ("id".to_string(), "nixpkgs".to_string()),
            ("type".to_string(), "indirect".to_string()),
        ]
        .drain(..)
        .collect::<BTreeMap<_, _>>();
        let expected = IndirectRef {
            _type: Tag::Indirect,
            id: "nixpkgs".to_string(),
            attributes: expected_attrs,
        };
        let actual_flakeref = FlakeRef::from_url("nixpkgs", PARSER_UTIL_BIN_PATH).unwrap();
        let expected_flakeref = FlakeRef::Indirect(expected);
        assert_eq!(actual_flakeref, expected_flakeref);
    }

    #[test]
    fn parses_indirect_ref() {
        let expected = IndirectRef {
            _type: Tag::Indirect,
            id: "nixpkgs".to_string(),
            attributes: BTreeMap::default(),
        };
        let actual = IndirectRef::from_str("flake:nixpkgs").unwrap();

        assert_eq!(actual, expected);
    }

    #[test]
    fn does_not_parse_other() {
        IndirectRef::from_str("github:nixpkgs").unwrap_err();
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
