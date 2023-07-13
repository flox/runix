use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::path::{Path, PathBuf};
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::Url;

use super::lock::{LastModified, Rev, RevCount};
use super::protocol::{self, Protocol, WrappedUrl, WrappedUrlParseError};
use super::{FlakeRefSource, Timestamp, TimestampDeserialize};

pub type GitUrl<Protocol> = WrappedUrl<Protocol>;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/git.cc#L287>
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
#[serde(tag = "type", rename = "git")]
pub struct GitRef<Protocol: GitProtocol> {
    pub url: GitUrl<Protocol>,

    #[serde(flatten)]
    pub attributes: GitAttributes,
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone, Default)]
pub struct GitAttributes {
    pub shallow: Option<bool>,
    pub submodules: Option<bool>,
    #[serde(rename = "allRefs")]
    pub all_refs: Option<bool>,

    #[serde(rename = "revCount")]
    pub rev_count: Option<RevCount>,

    #[serde(flatten)]
    pub rev: Option<Rev>,

    #[serde(rename = "ref")]
    pub reference: Option<String>,

    pub dir: Option<PathBuf>,

    #[serde(rename = "lastModified")]
    pub last_modified: Option<LastModified>,
}

pub trait GitProtocol: Protocol + Debug {}
impl GitProtocol for protocol::File {}
impl GitProtocol for protocol::SSH {}
impl GitProtocol for protocol::HTTP {}
impl GitProtocol for protocol::HTTPS {}

impl<Protocol: GitProtocol> GitRef<Protocol> {
    pub fn new(url: GitUrl<Protocol>, attributes: GitAttributes) -> Self {
        Self { url, attributes }
    }
}

impl<Protocol: GitProtocol> FlakeRefSource for GitRef<Protocol> {
    type ParseErr = ParseGitError;

    fn scheme() -> Cow<'static, str> {
        format!("git+{inner}", inner = Protocol::scheme()).into()
    }

    fn from_url(url: Url) -> Result<Self, Self::ParseErr> {
        if url.scheme() != Self::scheme() {
            return Err(ParseGitError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        }

        let mut pairs = url
            .query_pairs()
            .map(|(k, v)| (k, v.to_string()))
            .collect::<HashMap<_, _>>();

        let attributes = GitAttributes {
            shallow: pairs.remove("shallow").map(|v| v == "1"),
            submodules: pairs.remove("submodules").map(|v| v == "1"),
            all_refs: pairs.remove("allRefs").map(|v| v == "1"),
            rev_count: pairs
                .remove("revCount")
                .map(|v| v.parse::<u64>())
                .map_or(Ok(None), |v| v.map(Some))
                .map_err(|e| ParseGitError::Query(e.to_string()))?
                .map(RevCount),
            rev: pairs
                .remove("rev")
                .map(|v| Rev::from_str(&v))
                .map_or(Ok(None), |v| v.map(Some))
                .map_err(|e| ParseGitError::Query(e.to_string()))?,
            reference: pairs.remove("ref"),
            dir: pairs.remove("dir").map(PathBuf::from),
            last_modified: pairs
                .remove("lastModified")
                .map(|v| Timestamp::try_from(TimestampDeserialize::TsString(v)))
                .map_or(Ok(None), |v| v.map(Some))
                .map_err(|e| ParseGitError::Query(e.to_string()))?,
        };

        // Special Urls (File, Http, Https, Ftp) are by spec required to be absolute
        // since we are storing the internal Url in a spec compliant way,
        // we need to canonicalize/resolve relative paths.
        // This differs from the implementation found in nix which uses a non-spec-compliant
        // url implementation which supports `file:relative/path` urls
        // <https://github.com/NixOS/nix/blob/bf7dc3c7dc24f75fa623135750e8f10b8bcd94f9/src/libfetchers/git.cc#L261>
        let url = {
            let mut inner_url = Url::parse(url.as_str().trim_start_matches("git+"))?;

            let mut path = Path::new(url.path()).to_path_buf();
            if path.is_relative() {
                path = path
                    .canonicalize()
                    .map_err(|e| ParseGitError::Canonicalize(path, e))?;
            }

            inner_url.set_path(&format!("{}", path.to_string_lossy()));

            GitUrl::<Protocol>::try_from(inner_url)?
        };

        Ok(GitRef { url, attributes })
    }
}

impl<Protocol: GitProtocol> Display for GitRef<Protocol> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut url = Url::parse(&format!("git+{url}", url = self.url)).unwrap();

        let mut pairs = url.query_pairs_mut();

        if let Some(v) = self.attributes.all_refs {
            pairs.append_pair("allRefs", &(v as u8).to_string());
        }
        if let Some(ref v) = self.attributes.dir {
            pairs.append_pair("dir", &v.to_string_lossy());
        }
        if let Some(ref v) = self.attributes.last_modified {
            pairs.append_pair("lastModified", &v.0.timestamp().to_string());
        }
        if let Some(ref v) = self.attributes.reference {
            pairs.append_pair("ref", v);
        }
        if let Some(ref v) = self.attributes.rev {
            pairs.append_pair("rev", v);
        }
        if let Some(ref v) = self.attributes.rev_count {
            pairs.append_pair("revCount", &v.0.to_string());
        }
        if let Some(v) = self.attributes.shallow {
            pairs.append_pair("shallow", &(v as u8).to_string());
        }
        if let Some(v) = self.attributes.submodules {
            pairs.append_pair("submodules", &(v as u8).to_string());
        }

        let url = pairs.finish();

        write!(f, "{url}")
    }
}

impl<Protocol: GitProtocol> FromStr for GitRef<Protocol> {
    type Err = ParseGitError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;
        Self::from_url(url)
    }
}

#[derive(Debug, Error)]
pub enum ParseGitError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error(transparent)]
    GitUrl(#[from] WrappedUrlParseError),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("No repo specified")]
    NoRepo,
    #[error("Couldn't parse query: {0}")]
    Query(String),
    #[error("Could not resolve relative path '{0:?}': {1}")]
    Canonicalize(PathBuf, std::io::Error),
}

#[cfg(test)]
mod tests {

    use chrono::{TimeZone, Utc};
    use serde_json::json;

    use super::*;
    use crate::flake_ref::FlakeRef;

    static FLAKE_REF: &'_ str = "git+file:///somewhere/on/the/drive?dir=abc&lastModified=1666570118&ref=feature%2Fxyz&shallow=0&submodules=0";

    #[test]
    fn parses_git_path_flakeref() {
        let expected: GitRef<protocol::File> = GitRef {
            url: "file:///somewhere/on/the/drive".parse().unwrap(),
            attributes: GitAttributes {
                shallow: Some(false),
                submodules: Some(false),
                all_refs: None,
                rev_count: None,
                dir: Some("abc".into()),
                rev: None,
                last_modified: Some(Utc.timestamp_opt(1666570118, 0).unwrap().into()),
                reference: Some("feature/xyz".to_string()),
            },
        };

        assert_eq!(GitRef::from_str(FLAKE_REF).unwrap(), expected);
        assert_eq!(expected.to_string(), FLAKE_REF);
    }

    #[test]
    fn parses_unsescaped_qs() {
        assert_eq!(
            "git+file:///somewhere/on/the/drive?shallow=0&submodules=0&ref=feature/xyz&dir=abc&lastModified=1666570118"
                .parse::<GitRef<protocol::File>>()
                .unwrap()
                .to_string(),
            FLAKE_REF)
    }

    #[test]
    fn parses_bools() {
        let expected: GitRef<protocol::File> = GitRef {
            url: "file:///somewhere/on/the/drive".parse().unwrap(),
            attributes: GitAttributes {
                shallow: Some(false),
                submodules: Some(true),
                all_refs: Some(false), // sic only "1" is parsed as true
                ..Default::default()
            },
        };
        assert_eq!(
            "git+file:///somewhere/on/the/drive?shallow=0&submodules=1&allRefs=true"
                .parse::<GitRef<protocol::File>>()
                .unwrap(),
            expected
        )
    }

    #[test]
    fn to_from_json() {
        let expected = json!({
            "type": "git",
            "url": "ssh://git@github.com/flox/runix",
        });
        let flakeref = GitRef::<protocol::SSH> {
            url: "ssh://git@github.com/flox/runix".parse().unwrap(),
            attributes: Default::default(),
        };

        assert_eq!(
            serde_json::to_string(&flakeref).unwrap(),
            serde_json::to_string(&expected).unwrap()
        );
        assert_eq!(
            serde_json::from_value::<GitRef<protocol::SSH>>(expected).unwrap(),
            flakeref
        );
    }

    #[test]
    fn from_json() {
        let expected = json!({
          "lastModified": 1688730350,
          "narHash": "sha256-Gzcv5BkK4SIQVbxqMLxIBbJJcC0k6nGjgfve0X5lSzw=",
          "ref": "refs/heads/main",
          "rev": "0630fc9307852b30ea4c5915b6b74fa9db51d641",
          "revCount": 542,
          "type": "git",
          "url": "ssh://git@github.com/flox/flox",
          "shallow": true,
          "submodules": false,
        });
        serde_json::from_value::<GitRef<protocol::SSH>>(expected.clone()).expect("should parse");
        serde_json::from_value::<FlakeRef>(expected).expect("should parse");
    }

    /// assert that relative file urls are resolved to git urls correctly
    #[test]
    fn relative_git_urls() {
        let url = GitRef::<protocol::File>::from_str("git+file:..").unwrap();
        assert_eq!(
            url.url.path(),
            std::env::current_dir()
                .unwrap()
                .parent()
                .unwrap()
                .to_string_lossy()
        );
        dbg!(url.to_string());

        let url = GitRef::<protocol::File>::from_str("git+file:../").unwrap();
        dbg!(&url);

        assert_eq!(
            url.url.path(),
            std::env::current_dir()
                .unwrap()
                .parent()
                .unwrap()
                .to_string_lossy()
        );
        dbg!(url.to_string());

        // with "//" an absolute path is required per the Url spec for file urls
        let url = GitRef::<protocol::File>::from_str("git+file:///var/www").unwrap();
        dbg!(&url);
        assert_eq!(url.url.path(), "/var/www");
        dbg!(url.to_string());

        // with "//" an absolute path is required per the Url spec for file urls
        // relative paths such as var/www below are parsed as
        //
        //     var/www
        //        ^^^^ path
        //     ^^^ host
        // This also affects `./some/folder` where `.` will be interpreted as a host
        let url = GitRef::<protocol::File>::from_str("git+file://var/www").unwrap();
        dbg!(&url);
        assert_ne!(url.url.path(), "/var/www");
        assert_eq!(url.url.host_str(), Some("var"));
        assert_eq!(url.url.path(), "/www");

        dbg!(url.to_string());
    }
}
