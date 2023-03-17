//! A work in progress FlakeRef implementation
//!
//! Parses flake reference Urls as defined by the Nix reference implementation.

use std::collections::{BTreeMap, HashMap};
use std::num::ParseIntError;
use std::path::{Path, PathBuf};
use std::str::{FromStr, ParseBoolError};

use log::info;
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::form_urlencoded::Serializer;
use url::{Url, UrlQuery};

#[derive(Debug, Error)]
pub enum UrlError {
    #[error("Could not extract path from Url")]
    ExtractPath(()),
}

#[derive(Debug, Error)]
pub enum FlakeRefError {
    #[error("Url action failed: {0}")]
    UrlAccess(#[from] UrlError),

    #[error("Invalid FlakeRef Url: {0}")]
    FlakeRefUrl(Url),

    #[error("Could not parse flakeRef {0} as Url: {1}")]
    ParseUrl(String, url::ParseError),

    #[error("Could not parse `lastModified` field as Integer")]
    ParseLastModified(ParseIntError),
    #[error("Could not parse `revCount` field as Integer")]
    ParseRevCount(ParseIntError),

    #[error("Could not parse `shallow` field: {0}")]
    ParseShallow(ParseBoolError),
    #[error("Could not parse `submodules` field: {0}")]
    ParseSumbodules(ParseBoolError),
    #[error("Could not parse `allRefs` field: {0}")]
    ParseAllRefs(ParseBoolError),
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndirectFlake {
    pub id: FlakeId,
    pub args: BTreeMap<String, String>,
}

/// Flake ref definitions
/// TODO: make sure to conform with https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html
#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "type")]

pub enum ToFlakeRef {
    GitHub(GitService),
    GitLab(GitService),
    Sourcehut(GitService),
    /// https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/path.cc#L46
    Path {
        path: PathBuf,

        #[serde(rename = "revCount")]
        rev_count: Option<RevCount>,

        #[serde(flatten)]
        pinned: Option<Pinned>,
    },
    /// https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/git.cc#L287
    Git {
        url: Url,
        shallow: Option<bool>,
        submodules: Option<bool>,
        #[serde(rename = "allRefs")]
        all_refs: Option<bool>,

        #[serde(rename = "ref")]
        commit_ref: Option<CommitRef>,

        #[serde(rename = "revCount")]
        rev_count: Option<RevCount>,

        #[serde(flatten)]
        pinned: Option<Pinned>,

        dir: Option<PathBuf>,
    },
    /// https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/tarball.cc#L206
    Tarball {
        url: TarUrl,
        unpack: Option<bool>,
        #[serde(rename = "narHash")]
        nar_hash: NarHash,
    },
    Indirect(IndirectFlake),
}

impl ToFlakeRef {
    pub fn to_url(&self) -> Result<FlakeUrl, FlakeRefError> {
        let url = match self {
            ToFlakeRef::GitHub(e) | ToFlakeRef::GitLab(e) | ToFlakeRef::Sourcehut(e) => {
                let service = match self {
                    ToFlakeRef::GitHub(_) => "github",
                    ToFlakeRef::GitLab(_) => "gitlab",
                    ToFlakeRef::Sourcehut(_) => "sourcehut",
                    _ => unreachable!(),
                };

                let mut url = Url::parse(&format!("{service}:/"))
                    .expect("Failed initializing `{service}:` url");
                e.add_to_url(&mut url);
                url
            },
            ToFlakeRef::Path {
                path,
                rev_count,
                pinned,
            } => {
                // Ugly way to force "path" scheme and correct path
                let mut url = Url::parse("path:/").expect("Failed initializing `path:` url");

                // set the path part
                url.set_path(&path.to_string_lossy());

                // get the query handle
                let mut query = url.query_pairs_mut();

                if let Some(count) = rev_count {
                    query.append_pair("revCount", &count.to_string());
                }

                // add common `pin` attrbutes to query
                for pin in pinned {
                    pin.add_to_query(&mut query);
                }

                let url = query.finish().to_owned();

                debug_assert_eq!(url.scheme(), "path");
                url
            },
            ToFlakeRef::Git {
                url: _,
                shallow: _,
                submodules: _,
                all_refs: _,
                commit_ref: _,
                rev_count: _,
                pinned: _,
                dir: _,
            } => todo!(),
            ToFlakeRef::Tarball {
                url: _,
                unpack: _,
                nar_hash: _,
            } => todo!(),
            ToFlakeRef::Indirect(IndirectFlake { id, args }) => {
                let mut url = Url::parse(&format!("flake:{id}"))
                    .expect("Failed to create indirect reference");

                // append query
                {
                    let mut query = url.query_pairs_mut();
                    query.extend_pairs(args.iter());
                    query.finish();
                }

                url
            },
        };
        Ok(url)
    }

    pub fn from_url(url: &FlakeUrl) -> Result<ToFlakeRef, FlakeRefError> {
        let flake_ref = match url.scheme() {
            // https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/path.cc#L11
            "path" | "file" => ToFlakeRef::Path {
                path: PathBuf::from(url.path()),
                rev_count: url
                    .query_pairs()
                    .find(|(name, _)| name == "revCount")
                    .map(|(c, _)| c.parse().map_err(FlakeRefError::ParseRevCount))
                    .transpose()?,
                pinned: Pinned::from_query(url)?,
            },
            "github" => ToFlakeRef::GitHub(GitService::from_url(url)?),
            "flake" => ToFlakeRef::Indirect(IndirectFlake {
                id: url.path().to_string(),
                args: url
                    .query_pairs()
                    .map(|(k, v)| (k.into_owned(), v.into_owned()))
                    .collect(),
            }),
            "git+http" | "git+https" | "git+ssh" | "git+file" => {
                let pairs: HashMap<_, _> = url.query_pairs().collect();
                let shallow = pairs
                    .get("shallow")
                    .map(|shallow| shallow.parse())
                    .transpose()
                    .map_err(FlakeRefError::ParseShallow)?;
                let submodules = pairs
                    .get("submodules")
                    .map(|submodules| submodules.parse())
                    .transpose()
                    .map_err(FlakeRefError::ParseSumbodules)?;
                let all_refs = pairs
                    .get("all_refs")
                    .map(|all_refs| all_refs.parse())
                    .transpose()
                    .map_err(FlakeRefError::ParseAllRefs)?;

                let commit_ref = pairs.get("ref").map(|commit_ref| commit_ref.to_string());
                let rev_count = pairs
                    .get("revCount")
                    .map(|rev_count| rev_count.parse())
                    .transpose()
                    .map_err(FlakeRefError::ParseRevCount)?;
                let pinned = Pinned::from_query(url)?;
                let dir = pairs
                    .get("dir")
                    .map(|dir| Path::new(dir.as_ref()).to_path_buf());

                let wrapped_url = {
                    let mut url = url.clone();
                    url.set_query(None);
                    url.set_fragment(None);
                    Url::parse(url.to_string().trim_start_matches("git+")).unwrap()
                };

                ToFlakeRef::Git {
                    url: wrapped_url,
                    shallow,
                    submodules,
                    all_refs,
                    commit_ref,
                    rev_count,
                    pinned,
                    dir,
                }
            },
            _ => todo!(),
        };
        Ok(flake_ref)
    }
}

impl FromStr for ToFlakeRef {
    type Err = FlakeRefError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s).or_else(|e| {
            if s.starts_with('.') || s.starts_with('/') {
                info!("could not parse '{s}' as qualified url, trying to parse as `path:` ({e})",);
                Url::parse(&format!("path://{s}"))
                    .map_err(|e| FlakeRefError::ParseUrl(s.to_string(), e))
            } else {
                info!("could not parse '{s}' as qualified url, trying to parse as `flake:` ({e})",);
                Url::parse(&format!("flake:{s}"))
                    .map_err(|e| FlakeRefError::ParseUrl(s.to_string(), e))
            }
        })?;

        ToFlakeRef::from_url(&url)
    }
}

#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub enum Pinned {
    NarAndRev {
        #[serde(rename = "narHash")]
        nar_hash: NarHash,
        #[serde(rename = "lastModified")]
        last_modified: LastModified,
        #[serde(rename = "rev")]
        commit_rev: CommitRev,
    },
    Nar {
        #[serde(rename = "narHash")]
        nar_hash: NarHash,
        #[serde(rename = "lastModified")]
        last_modified: LastModified,
    },

    Rev {
        #[serde(rename = "rev")]
        commit_rev: CommitRev,

        #[serde(rename = "lastModified")]
        last_modified: LastModified,
    },
}

impl Pinned {
    fn add_to_query(&self, query: &mut Serializer<UrlQuery>) {
        match self {
            Pinned::Nar {
                nar_hash,
                last_modified: _,
            } => {
                query.append_pair("narHash", &nar_hash.clone());
            },
            Pinned::Rev {
                commit_rev,
                last_modified: _,
            } => {
                query.append_pair("rev", &commit_rev.clone());
            },
            Pinned::NarAndRev {
                nar_hash,
                last_modified: _,
                commit_rev,
            } => {
                query.append_pair("narHash", &nar_hash.clone());
                query.append_pair("rev", &commit_rev.clone());
            },
        };

        match self {
            Pinned::NarAndRev { last_modified, .. }
            | Pinned::Nar { last_modified, .. }
            | Pinned::Rev { last_modified, .. } => {
                query.append_pair("lastModified", &last_modified.to_string())
            },
        };
    }

    fn from_query(url: &Url) -> Result<Option<Self>, FlakeRefError> {
        let query: HashMap<_, _> = url.query_pairs().collect();

        let nar_hash = query.get("narHash");
        let rev = query.get("rev");
        let last_modified = query
            .get("lastModified")
            .and_then(|s| s.parse().ok())
            .unwrap_or_default();

        let pinned = match (nar_hash, rev) {
            (None, None) => None,
            (None, Some(rev)) => Some(Self::Rev {
                commit_rev: rev.to_string(),
                last_modified,
            }),
            (Some(nar), None) => Some(Self::Nar {
                nar_hash: nar.to_string(),
                last_modified,
            }),
            (Some(nar), Some(rev)) => Some(Self::NarAndRev {
                nar_hash: nar.to_string(),
                last_modified,
                commit_rev: rev.to_string(),
            }),
        };

        Ok(pinned)
    }
}

/// Encodes type github type (+ gitlab + sourcehut = git service) data
///
/// https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/github.cc#L108
#[skip_serializing_none]
#[derive(Debug, Clone, Default, Deserialize, Serialize, PartialEq, Eq)]
pub struct GitService {
    owner: RepoOwner,
    repo: RepoName,
    host: Option<RepoHost>,
    #[serde(rename = "ref")]
    commit_ref: Option<CommitRef>,
    #[serde(flatten)]
    pinned: Option<Pinned>,

    dir: Option<PathBuf>,
}

impl GitService {
    fn add_to_url(&self, url: &mut Url) {
        let path = format!("{}/{}", self.owner, self.repo);
        url.set_path(&path);
        let mut query = url.query_pairs_mut();
        if let Some(ref commit_ref) = self.commit_ref {
            query.append_pair("ref", commit_ref);
        }
        for pin in &self.pinned {
            pin.add_to_query(&mut query);
        }
        query.finish();
    }

    fn from_url(url: &Url) -> Result<Self, FlakeRefError> {
        let mut service = match url
            .path()
            .splitn(3, '/')
            // .cloned()
            .collect::<Vec<_>>()[..]
        {
            ["", owner, repo] | [owner, repo] => GitService {
                owner: owner.to_string(),
                repo: repo.to_string(),
                ..Default::default()
            },
            ["", owner, repo, commit_ref] | [owner, repo, commit_ref] => GitService {
                owner: owner.to_string(),
                repo: repo.to_string(),
                commit_ref: Some(commit_ref.to_string()),
                ..Default::default()
            },
            _ => Err(FlakeRefError::FlakeRefUrl(url.clone()))?,
        };

        if let Some((_, commit_ref)) = url.query_pairs().find(|(name, _)| name == "ref") {
            let _ = service.commit_ref.insert(commit_ref.to_string());
        }

        Ok(service)
    }
}

pub type FlakeId = String;
pub type RepoOwner = String;
pub type RepoName = String;
pub type CommitRef = String;
pub type CommitRev = String;
pub type RepoHost = String;
pub type NarHash = String;
pub type LastModified = u64;
pub type RevCount = u64;
pub type GitUrl = String;
pub type TarUrl = String;
pub type FlakeUrl = Url;

#[cfg(test)]
mod tests {

    use url::Url;

    use super::*;

    #[test]
    fn parses_dot_flakeref() {
        ToFlakeRef::from_str(".").unwrap();
        ToFlakeRef::from_str("path:.").unwrap();
    }

    #[test]
    fn parses_path_flakeref() {
        dbg!(ToFlakeRef::from_str("/").unwrap());

        assert_eq!(
            ToFlakeRef::from_str("/some/where").unwrap(),
            ToFlakeRef::from_str("path:///some/where").unwrap()
        );
        assert_eq!(
            ToFlakeRef::from_str("/some/where").unwrap(),
            ToFlakeRef::from_str("path:/some/where").unwrap()
        );
    }

    #[test]
    fn parses_registry_flakeref() {
        let expected = ToFlakeRef::Indirect(IndirectFlake {
            id: "nixpkgs".to_string(),
            args: BTreeMap::default(),
        });

        assert_eq!(&ToFlakeRef::from_str("flake:nixpkgs").unwrap(), &expected);
        assert_eq!(&ToFlakeRef::from_str("nixpkgs").unwrap(), &expected);
    }

    #[test]
    fn parses_git_path_flakeref() {
        let expected = ToFlakeRef::Git {
            url: Url::from_directory_path("/somewhere/on/the/drive").unwrap(),
            shallow: Some(false),
            submodules: Some(false),
            all_refs: None,
            commit_ref: Some("feature/xyz".to_owned()),
            rev_count: None,
            pinned: None,
            dir: Some("abc".into()),
        };

        assert_eq!(
            &ToFlakeRef::from_str(
                "git+file:///somewhere/on/the/drive/?shallow=false&submodules=false&ref=feature/xyz&dir=abc"
            )
            .unwrap(),
            &expected
        );
    }

    #[test]
    fn parses_github_flakeref() {
        let flakeref = serde_json::from_str::<ToFlakeRef>(
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

        assert_eq!(
            flakeref,
            ToFlakeRef::GitHub(GitService {
                owner: "flox".into(),
                repo: "nixpkgs".into(),
                host: None,
                commit_ref: Some("unstable".into()),
                pinned: None,
                dir: None,
            })
        )
    }

    #[test]
    fn parses_pinned_path() {
        let flakeref = serde_json::from_str::<ToFlakeRef>(
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

        assert_eq!(flakeref, ToFlakeRef::Path {
            path: "/nix/store/083m43hjhry94cvfmqdv7kjpvsl3zzvi-source".into(),
            rev_count: None,
            pinned: Some(Pinned::NarAndRev {
                nar_hash: "sha256-MTXmIYowHM1wyIYyqPdBLia5SjGnxETv0YkIbDsbkx4=".into(),
                last_modified: 1666570118,
                commit_rev: "1e684b371cf05300bc2b432f958f285855bac8fb".into()
            })
        })
    }

    /// Ensure that a path flake ref serializes without information loss
    #[test]
    fn path_to_from_url() {
        let flake_ref = ToFlakeRef::Path {
            path: "/nix/store/083m43hjhry94cvfmqdv7kjpvsl3zzvi-source".into(),
            rev_count: None,
            pinned: Some(Pinned::NarAndRev {
                nar_hash: "sha256-MTXmIYowHM1wyIYyqPdBLia5SjGnxETv0YkIbDsbkx4=".into(),
                last_modified: 1666570118,
                commit_rev: "1e684b371cf05300bc2b432f958f285855bac8fb".into(),
            }),
        };

        let parsed = ToFlakeRef::from_url(&flake_ref.to_url().expect("should serialize to url"))
            .expect("should deserialize from url");

        assert_eq!(flake_ref, parsed)
    }

    /// Ensure that paths with `path` and `file` scheme parse
    #[test]
    fn path_from_path_url() {
        let qualified = Url::parse("path:/my/directory");
        assert!(dbg!(&qualified).is_ok());

        let unqualified = Url::from_file_path("/my/directory");
        assert!(dbg!(&unqualified).is_ok());

        assert_eq!(
            ToFlakeRef::from_url(&qualified.unwrap()).expect("Should parse qualified path"),
            ToFlakeRef::from_url(&unqualified.unwrap()).expect("Should parse unqualified path")
        )
    }

    /// Ensure that a github flake ref serializes without information loss
    #[test]
    fn github_to_from_url() {
        let flake_ref = ToFlakeRef::GitHub(GitService {
            owner: "flox".into(),
            repo: "nixpkgs".into(),
            host: None,
            commit_ref: Some("unstable".into()),
            pinned: None,
            dir: None,
        });

        let parsed = ToFlakeRef::from_url(&flake_ref.to_url().expect("should serialize to url"))
            .expect("should deserialize from url");

        assert_eq!(flake_ref, parsed)
    }

    /// Ensure that an indirect flake ref serializes without information loss
    #[test]
    fn indirect_to_from_url() {
        let flake_ref = ToFlakeRef::Indirect(IndirectFlake {
            id: "nixpkgs-flox".into(),
            args: BTreeMap::default(),
        });

        let parsed = ToFlakeRef::from_url(&flake_ref.to_url().expect("should serialize to url"))
            .expect("should deserialize from url");

        assert_eq!(flake_ref, parsed)
    }
}
