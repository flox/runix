use std::borrow::Cow;
use std::fmt::Display;
use std::ops::Deref;
use std::path::PathBuf;
use std::str::FromStr;

use serde::{Deserialize, Serialize};
use thiserror::Error;
use url::Url;

use self::service::GitService;
use super::lock::RevOrRef;
use super::FlakeRefSource;
use crate::flake_ref::{CommitRef, NarHash, RepoHost};

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq)]
pub struct GitServiceSource<Service> {
    owner: String,
    repo: String,

    #[serde(flatten)]
    attributes: GitServiceAttributes,

    #[serde(rename = "type")]
    #[serde(bound(deserialize = "GitService<Service>: Deserialize<'de>"))]
    #[serde(bound(serialize = "GitService<Service>: Serialize"))]
    _type: GitService<Service>,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq, Eq)]
pub struct GitServiceAttributes {
    host: Option<RepoHost>,
    dir: Option<PathBuf>,

    #[serde(rename = "ref")]
    commit_ref: Option<CommitRef>,

    rev_or_ref: Option<RevOrRef>,

    #[serde(flatten)]
    nar_hash: Option<NarHash>,

    #[serde(flatten)]
    last_modified: Option<NarHash>,
}

pub mod service {
    use std::borrow::Cow;

    use derive_more::From;
    use serde::{Deserialize, Serialize};

    #[derive(Default, Debug, PartialEq, Eq, From)]
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

    #[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
    pub struct Github;
    impl GitServiceHost for Github {
        fn scheme() -> Cow<'static, str> {
            "github".into()
        }
    }

    #[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
    pub struct Gitlab;

    impl GitServiceHost for Gitlab {
        fn scheme() -> Cow<'static, str> {
            "gitlab".into()
        }
    }
}

impl<Service: service::GitServiceHost> FlakeRefSource for GitServiceSource<Service> {
    fn scheme() -> Cow<'static, str> {
        Service::scheme()
    }
}
impl<Service: service::GitServiceHost> FromStr for GitServiceSource<Service> {
    type Err = ParseGitServiceError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;
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
            Some((repo, rev)) => (repo, Some(rev.to_owned().into())),
            None => (rest, None),
        };

        let mut attributes: GitServiceAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or("")).unwrap();

        if attributes.rev_or_ref.is_some() && rev_or_ref.is_some() {
            return Err(ParseGitServiceError::TwoRevs);
        }

        attributes.rev_or_ref = attributes.rev_or_ref.or(rev_or_ref);

        Ok(GitServiceSource {
            owner: owner.to_string(),
            repo: repo.to_string(),
            attributes,
            _type: Default::default(),
        })
    }
}
impl<Service: service::GitServiceHost> Display for GitServiceSource<Service> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut attributes = self.attributes.clone();

        write!(
            f,
            "{schema}:{owner}/{repo}",
            schema = Self::scheme(),
            owner = self.owner,
            repo = self.repo
        )?;
        if let Some(rev_or_ref) = attributes.rev_or_ref.take() {
            let part = match rev_or_ref {
                RevOrRef::Rev { ref rev } => rev.deref(),
                RevOrRef::Ref { ref reference } => reference,
            };
            write!(f, "/{part}")?;
        };

        write!(
            f,
            "{query}",
            query = serde_urlencoded::to_string(attributes).unwrap()
        )?;
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ParseGitServiceError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("Url contains multiple commit hashes")]
    TwoRevs,
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("No repo specified")]
    NoRepo,
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn parse_github_simple() {
        GitServiceSource::<service::Github>::from_str("github:owner/repo").unwrap();
    }

    #[test]
    fn fail_parse_github_no_repo() {
        assert!(matches!(
            GitServiceSource::<service::Github>::from_str("github:owner"),
            Err(ParseGitServiceError::NoRepo)
        ))
    }

    #[test]
    fn parse_attributes() {
        assert_eq!(
            GitServiceSource::<service::Github>::from_str(
                "github:owner/repo?ref=abcdef&dir=subdir"
            )
            .unwrap(),
            GitServiceSource {
                owner: "owner".to_string(),
                repo: "repo".to_string(),
                attributes: GitServiceAttributes {
                    host: None,
                    dir: Some(Path::new("subdir").to_path_buf()),
                    commit_ref: Some("abcdef".to_string()),
                    nar_hash: None,
                    last_modified: None,
                    rev_or_ref: None,
                },
                _type: GitService::default()
            }
        );
    }

    #[test]
    fn parses_github_flakeref() {
        let flakeref = serde_json::from_str::<GitServiceSource<service::Github>>(
            r#"
{
    "owner": "flox",
    "ref": "unstable",
    "repo": "nixpkgs",
    "type": "github"
}
        "#,
        )
        .expect("should parse");

        assert_eq!(flakeref, GitServiceSource {
            owner: "flox".into(),
            repo: "nixpkgs".into(),
            attributes: GitServiceAttributes {
                host: None,
                commit_ref: Some("unstable".into()),
                dir: None,
                rev_or_ref: None,
                nar_hash: None,
                last_modified: None
            },
            _type: GitService::default(),
        });
    }
}
