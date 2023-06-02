use std::borrow::Cow;
use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::Url;

use super::lock::{LastModified, NarHash, Rev, RevCount};
use super::FlakeRefSource;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/path.cc#L46>
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(tag = "path")]
pub struct PathRef {
    pub path: PathBuf,
    #[serde(flatten)]
    pub attributes: PathAttributes,
}

#[skip_serializing_none]
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
pub struct PathAttributes {
    #[serde(rename = "revCount")]
    pub rev_count: Option<RevCount>,

    #[serde(rename = "narHash")]
    pub nar_hash: Option<NarHash>,

    #[serde(rename = "lastModified")]
    pub last_modified: Option<LastModified>,

    pub rev: Option<Rev>,
}

impl PathRef {
    pub fn new(path: PathBuf, attributes: PathAttributes) -> Self {
        Self { path, attributes }
    }
}

impl FlakeRefSource for PathRef {
    type ParseErr = ParsePathRefError;

    fn scheme() -> Cow<'static, str> {
        "path".into()
    }

    fn parses(maybe_ref: &str) -> bool {
        ["path:", "/", "."]
            .iter()
            .any(|prefix| maybe_ref.starts_with(prefix))
    }

    fn from_url(url: Url) -> Result<Self, Self::ParseErr> {
        if url.scheme() != Self::scheme() {
            return Err(ParsePathRefError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        }
        let path = Path::new(url.path()).to_path_buf();
        let attributes: PathAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or_default())?;

        Ok(PathRef { path, attributes })
    }
}

impl FromStr for PathRef {
    type Err = ParsePathRefError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = match Url::parse(s) {
            Ok(url) => url,
            Err(_) if Self::parses(s) && !s.starts_with(&*Self::scheme()) => {
                Url::parse(&format!("path:{s}"))?
            },
            e => e?,
        };
        Self::from_url(url)
    }
}

impl Display for PathRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut url: Url = format!(
            "{scheme}:{path}",
            scheme = Self::scheme(),
            path = self.path.to_string_lossy(),
        )
        .parse()
        .unwrap();

        url.set_query(
            serde_urlencoded::to_string(&self.attributes)
                .ok()
                .as_deref(),
        );

        write!(f, "{url}")
    }
}

#[derive(Debug, Error)]
pub enum ParsePathRefError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("Couldn't parse query: {0}")]
    Query(#[from] serde_urlencoded::de::Error),
}

#[cfg(test)]
mod tests {
    use chrono::{TimeZone, Utc};

    use super::*;

    #[test]
    fn parses_path_flakeref() {
        assert_eq!(
            PathRef::from_str("/some/where").unwrap(),
            PathRef::from_str("path:///some/where").unwrap()
        );
        assert_eq!(
            PathRef::from_str("./some/where").unwrap(),
            PathRef::from_str("path:./some/where").unwrap()
        );
    }

    #[test]
    fn parses_pinned_path() {
        let flakeref = serde_json::from_str::<PathRef>(
            r#"
{
    "lastModified": 1666570118,
    "narHash": "sha256-MTXmIYowHM1wyIYyqPdBLia5SjGnxETv0YkIbDsbkx4=",
    "path": "/nix/store/083m43hjhry94cvfmqdv7kjpvsl3zzvi-source",
    "rev": "1e684b371cf05300bc2b432f958f285855bac8fb",
    "type": "path"
}
        "#,
        )
        .expect("should parse pin");

        assert_eq!(flakeref, PathRef {
            path: "/nix/store/083m43hjhry94cvfmqdv7kjpvsl3zzvi-source".into(),
            attributes: PathAttributes {
                rev_count: None,
                nar_hash: Some("sha256-MTXmIYowHM1wyIYyqPdBLia5SjGnxETv0YkIbDsbkx4=".into()),
                last_modified: Some(Utc.timestamp_opt(1666570118, 0).unwrap().into()),
                rev: Some("1e684b371cf05300bc2b432f958f285855bac8fb".parse().unwrap())
            }
        })
    }

    /// Ensure that a path flake ref serializes without information loss
    #[test]
    fn path_to_from_url() {
        let flake_ref = PathRef {
            path: "/nix/store/083m43hjhry94cvfmqdv7kjpvsl3zzvi-source".into(),
            attributes: PathAttributes {
                rev_count: None,
                nar_hash: Some("sha256-MTXmIYowHM1wyIYyqPdBLia5SjGnxETv0YkIbDsbkx4=".into()),
                last_modified: Some(Utc.timestamp_opt(1666570118, 0).unwrap().into()),
                rev: Some("1e684b371cf05300bc2b432f958f285855bac8fb".parse().unwrap()),
            },
        };

        dbg!(&flake_ref);

        let serialized = flake_ref.to_string();
        dbg!(&serialized);
        let parsed = serialized.parse().unwrap();

        assert_eq!(flake_ref, parsed);
    }
}
