use std::borrow::Cow;
use std::fmt::Display;
use std::path::PathBuf;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::Url;

use super::lock::{Rev, RevCount};
use super::protocol::{self, Protocol, WrappedUrl, WrappedUrlParseError};
use super::FlakeRefSource;

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
}

pub trait GitProtocol: Protocol {}
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
    fn scheme() -> Cow<'static, str> {
        format!("git+{inner}", inner = Protocol::scheme()).into()
    }
}

impl<Protocol: GitProtocol> Display for GitRef<Protocol> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut url = Url::parse(&format!("git+{url}", url = self.url)).unwrap();
        url.set_query(
            serde_urlencoded::to_string(&self.attributes)
                .ok()
                .as_deref(),
        );

        write!(f, "{url}")
    }
}

impl<Protocol: GitProtocol> FromStr for GitRef<Protocol> {
    type Err = ParseGitError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;

        if url.scheme() != Self::scheme() {
            return Err(ParseGitError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        }

        let attributes: GitAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or_default())?;

        let url = GitUrl::<Protocol>::from_str(s.trim_start_matches("git+"))?;

        Ok(GitRef { url, attributes })
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
    Query(#[from] serde_urlencoded::de::Error),
}

#[cfg(test)]
mod tests {

    use serde_json::json;

    use super::*;

    static FLAKE_REF: &'_ str = "git+file:///somewhere/on/the/drive?shallow=false&submodules=false&ref=feature%2Fxyz&dir=abc";

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
                reference: Some("feature/xyz".to_string()),
            },
        };

        assert_eq!(GitRef::from_str(FLAKE_REF).unwrap(), expected);
        assert_eq!(expected.to_string(), FLAKE_REF);
    }

    #[test]
    fn parses_unsescaped_qs() {
        assert_eq!(
            "git+file:///somewhere/on/the/drive?shallow=false&submodules=false&ref=feature/xyz&dir=abc"
                .parse::<GitRef<protocol::File>>()
                .unwrap()
                .to_string(),
            FLAKE_REF)
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
}
