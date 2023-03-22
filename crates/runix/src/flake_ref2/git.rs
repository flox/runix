use std::borrow::Cow;
use std::fmt::Display;
use std::path::PathBuf;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use thiserror::Error;
use url::Url;

use super::lock::Rev;
use super::protocol::{self, Protocol, WrappedUrl, WrappedUrlParseError};
use super::FlakeRefSource;
use crate::flake_ref::RevCount;

pub type GitUrl<Protocol> = WrappedUrl<Protocol>;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/git.cc#L287>
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
#[serde(tag = "git")]
pub struct GitRef<Protocol: GitProtocol> {
    url: GitUrl<Protocol>,

    #[serde(flatten)]
    attributes: GitAttributes,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
pub struct GitAttributes {
    shallow: Option<bool>,
    submodules: Option<bool>,
    #[serde(rename = "allRefs")]
    all_refs: Option<bool>,

    #[serde(rename = "revCount")]
    rev_count: Option<RevCount>,

    #[serde(flatten)]
    rev: Option<Rev>,

    #[serde(rename = "ref")]
    reference: Option<String>,

    dir: Option<PathBuf>,
}

pub trait GitProtocol: Protocol {}
impl GitProtocol for protocol::File {}
impl GitProtocol for protocol::SSH {}
impl GitProtocol for protocol::HTTP {}
impl GitProtocol for protocol::HTTPS {}

impl<Protcol: GitProtocol> FlakeRefSource for GitRef<Protcol> {
    fn scheme() -> Cow<'static, str> {
        format!("git+{inner}", inner = Protcol::scheme()).into()
    }
}

impl<Protcol: GitProtocol> Display for GitRef<Protcol> {
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

impl<Protcol: GitProtocol> FromStr for GitRef<Protcol> {
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

        let url = GitUrl::<Protcol>::from_str(s.trim_start_matches("git+"))?;

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
}
