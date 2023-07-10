use std::borrow::Cow;
use std::fmt::Display;
use std::path::PathBuf;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::Url;

use self::service::GitService;
use super::lock::{LastModified, NarHash, Rev, RevOrRef};
use super::FlakeRefSource;

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
pub struct GitServiceRef<Service> {
    pub owner: String,
    pub repo: String,

    #[serde(flatten)]
    pub attributes: GitServiceAttributes,

    #[serde(rename = "type")]
    #[serde(bound(deserialize = "GitService<Service>: Deserialize<'de>"))]
    #[serde(bound(serialize = "GitService<Service>: Serialize"))]
    _type: GitService<Service>,
}

#[skip_serializing_none]
#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq, Default)]
#[serde(deny_unknown_fields)]
pub struct GitServiceAttributes {
    pub host: Option<String>,
    pub dir: Option<PathBuf>,

    #[serde(rename = "ref")]
    pub reference: Option<String>,

    pub rev: Option<Rev>,

    #[serde(rename = "narHash")]
    pub nar_hash: Option<NarHash>,

    #[serde(rename = "lastModified")]
    pub last_modified: Option<LastModified>,
}

pub mod service {
    use std::borrow::Cow;

    use derive_more::From;
    use serde::{Deserialize, Serialize};

    #[derive(Default, Debug, PartialEq, Eq, From, Clone)]
    pub struct GitService<Service>(Service);

    impl<Service: GitServiceHost> Serialize for GitService<Service> {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            serializer.serialize_str(&Service::scheme())
        }
    }

    impl<'de, Service: GitServiceHost> Deserialize<'de> for GitService<Service> {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            let s = String::deserialize(deserializer)?;
            if s != Service::scheme() {
                return Err(serde::de::Error::invalid_value(
                    serde::de::Unexpected::Str(&s),
                    &&*Service::scheme(),
                ));
            }

            Ok(Default::default())
        }
    }

    pub(crate) trait GitServiceHost: Default + Eq {
        fn scheme() -> Cow<'static, str>;
    }

    #[derive(Default, Debug, PartialEq, Eq, Clone)]
    pub struct Github;
    impl GitServiceHost for Github {
        fn scheme() -> Cow<'static, str> {
            "github".into()
        }
    }

    #[derive(Default, Debug, PartialEq, Eq, Clone)]
    pub struct Gitlab;
    impl GitServiceHost for Gitlab {
        fn scheme() -> Cow<'static, str> {
            "gitlab".into()
        }
    }
}

impl<Service: Default> GitServiceRef<Service> {
    pub fn new(owner: String, repo: String, attributes: GitServiceAttributes) -> Self {
        Self {
            owner,
            repo,
            attributes,
            _type: Default::default(),
        }
    }
}

impl<Service: service::GitServiceHost> FlakeRefSource for GitServiceRef<Service> {
    type ParseErr = ParseGitServiceError;

    fn scheme() -> Cow<'static, str> {
        Service::scheme()
    }

    fn from_url(url: Url) -> Result<Self, Self::ParseErr> {
        if url.scheme() != Self::scheme() {
            return Err(ParseGitServiceError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        }
        let (owner, rest) = url
            .path()
            .split_once('/')
            .ok_or(ParseGitServiceError::NoRepo)?;
        let (repo, rev_or_ref) = match rest.split_once('/') {
            Some((repo, rev_or_ref)) => (repo, Some(rev_or_ref.to_owned().into())),
            None => (rest, None),
        };

        let mut attributes: GitServiceAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or_default())?;

        if attributes.rev.is_some() && attributes.reference.is_some() {
            Err(ParseGitServiceError::TwoRevs)?;
        }

        if (attributes.rev.is_some() || attributes.reference.is_some()) && rev_or_ref.is_some() {
            Err(ParseGitServiceError::TwoRevs)?;
        }

        match rev_or_ref {
            Some(RevOrRef::Rev { rev }) => attributes.rev = Some(rev),
            Some(RevOrRef::Ref { reference }) => attributes.reference = Some(reference),
            None => {},
        }

        Ok(GitServiceRef {
            owner: owner.to_string(),
            repo: repo.to_string(),
            attributes,
            _type: Default::default(),
        })
    }
}
impl<Service: service::GitServiceHost> FromStr for GitServiceRef<Service> {
    type Err = ParseGitServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;
        Self::from_url(url)
    }
}

impl<Service: service::GitServiceHost> Display for GitServiceRef<Service> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut attributes = self.attributes.clone();

        write!(
            f,
            "{schema}:{owner}/{repo}",
            schema = Self::scheme(),
            owner = self.owner,
            repo = self.repo
        )?;

        if let Some(part) = attributes
            .rev
            .take()
            .map(|rev| rev.to_string())
            .or_else(|| attributes.reference.take())
        {
            write!(f, "/{part}")?;
        };

        let query = serde_urlencoded::to_string(attributes).unwrap_or_default();
        if !query.is_empty() {
            write!(f, "?{query}",)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ParseGitServiceError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("Url contains multiple commit hashes")]
    TwoRevs,
    #[error("Couldn't parse query: {0}")]
    Query(#[from] serde_urlencoded::de::Error),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("No repo specified")]
    NoRepo,
    #[error("Unkown Attribute: {0}")]
    UnkownAttribute(String),
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;
    use crate::flake_ref::tests::{roundtrip, roundtrip_to};

    #[test]
    fn parse_github_simple() {
        roundtrip::<GitServiceRef<service::Github>>("github:owner/repo");
        roundtrip::<GitServiceRef<service::Github>>("github:owner/repo/feature-branch");
        roundtrip_to::<GitServiceRef<service::Github>>(
            "github:owner/repo?ref=feature-branch",
            "github:owner/repo/feature-branch",
        );
        roundtrip_to::<GitServiceRef<service::Github>>(
            "github:owner/repo?rev=50500a744e3c2af9d89123ae17b71406b428c3ab",
            "github:owner/repo/50500a744e3c2af9d89123ae17b71406b428c3ab",
        );
    }

    #[test]
    fn parse_invalid_simple() {
        GitServiceRef::<service::Github>::from_str("github:owner/repo/feature?ref=another-feature")
            .unwrap_err();
        GitServiceRef::<service::Github>::from_str(
            "github:owner/repo/feature?rev=50500a744e3c2af9d89123ae17b71406b428c3ab",
        )
        .unwrap_err();
        GitServiceRef::<service::Github>::from_str(
            "github:owner/repo?ref=feature&rev=50500a744e3c2af9d89123ae17b71406b428c3ab",
        )
        .unwrap_err();
    }

    #[test]
    fn fail_parse_github_no_repo() {
        assert!(matches!(
            GitServiceRef::<service::Github>::from_str("github:owner"),
            Err(ParseGitServiceError::NoRepo)
        ))
    }

    #[test]
    fn parse_attributes() {
        assert_eq!(
            GitServiceRef::<service::Github>::from_str("github:owner/repo/unstable?dir=subdir")
                .unwrap(),
            GitServiceRef {
                owner: "owner".to_string(),
                repo: "repo".to_string(),
                attributes: GitServiceAttributes {
                    host: None,
                    dir: Some(Path::new("subdir").to_path_buf()),
                    reference: Some("unstable".into()),
                    rev: None,
                    nar_hash: None,
                    last_modified: None,
                },
                _type: GitService::default()
            }
        );
    }

    #[test]
    fn parses_github_flakeref() {
        println!(
            "{}",
            serde_json::to_string_pretty(&GitServiceRef::<service::Github> {
                owner: "flox".into(),
                repo: "nixpkgs".into(),
                attributes: GitServiceAttributes {
                    host: None,
                    dir: None,
                    reference: Some("unstable".into()),
                    rev: None,
                    nar_hash: None,
                    last_modified: None,
                },
                _type: GitService::default(),
            })
            .unwrap()
        );

        let flakeref = serde_json::from_str::<GitServiceRef<service::Github>>(
            r#"
{
  "owner": "flox",
  "repo": "nixpkgs",
  "ref": "unstable",
  "type": "github"
}
        "#,
        )
        .expect("should parse");

        dbg!(&flakeref);

        assert_eq!(flakeref, GitServiceRef {
            owner: "flox".into(),
            repo: "nixpkgs".into(),
            attributes: GitServiceAttributes {
                host: None,
                dir: None,
                reference: Some("unstable".into()),
                rev: None,
                nar_hash: None,
                last_modified: None,
            },
            _type: GitService::default(),
        });
    }
}
