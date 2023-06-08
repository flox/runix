use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Display;
use std::os::unix::prelude::MetadataExt;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use chrono::{NaiveDateTime, TimeZone, Utc};
use derive_more::{Display, From};
use log::{debug, info};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Deserializer, Serialize};
use thiserror::Error;
use url::Url;

use self::file::{FileRef, TarballRef};
use self::git::GitRef;
use self::git_service::{service, GitServiceRef};
use self::indirect::IndirectRef;
use self::path::PathRef;

pub mod file;
pub mod git;
pub mod git_service;
pub mod indirect;
pub mod lock;
pub mod path;
pub mod protocol;

pub static FLAKE_ID_REGEX: Lazy<Regex> =
    Lazy::new(|| Regex::new("[a-zA-Z][a-zA-Z0-9_-]*").unwrap());

pub trait FlakeRefSource: FromStr + Display {
    type ParseErr;

    fn scheme() -> Cow<'static, str>;

    fn from_url(url: Url) -> Result<Self, Self::ParseErr>;

    fn parses(maybe_ref: &str) -> bool {
        maybe_ref.starts_with(&format!("{}:", Self::scheme()))
    }
}

#[derive(Serialize, Deserialize, Display, From, Debug, Clone, PartialEq, Eq)]
#[serde(untagged)]
pub enum FlakeRef {
    FileFile(FileRef<protocol::File>),
    FileHTTP(FileRef<protocol::HTTP>),
    FileHTTPS(FileRef<protocol::HTTPS>),
    TarballFile(TarballRef<protocol::File>),
    TarballHTTP(TarballRef<protocol::HTTP>),
    TarballHTTPS(TarballRef<protocol::HTTPS>),
    Github(GitServiceRef<service::Github>),
    Gitlab(GitServiceRef<service::Gitlab>),
    Path(PathRef),
    GitPath(GitRef<protocol::File>),
    GitSsh(GitRef<protocol::SSH>),
    GitHttps(GitRef<protocol::HTTPS>),
    GitHttp(GitRef<protocol::HTTP>),
    Indirect(IndirectRef),
    // /// https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/tarball.cc#L206
    // Tarball(TarballRef),
}

impl FromStr for FlakeRef {
    type Err = ParseFlakeRefError;

    /// Parse a flakeref string into a typed flakeref
    ///
    /// If not well defined, i.e. if the flakeref cannot be parsed as a url,
    /// we resolve it either as an indirect flake or local path.
    ///
    /// Note: if not "well-defined" parsing flakerefs is "impure",
    ///       i.e. depends on the state of the local system (files).
    ///       The resulting flakeref however, serializes into well-defined form.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = match Url::parse(s) {
            Ok(well_defined) => well_defined,
            Err(_) => {
                let resolved = if FLAKE_ID_REGEX.is_match(s) {
                    Url::parse(&format!("flake:{s}")).map_err(|e| {
                        ParseFlakeRefError::Indirect(indirect::ParseIndirectError::Url(e))
                    })?
                } else {
                    FlakeRef::resolve_local(s)?
                };

                info!("Could not parse flakeref as URL; resolved locally to '{resolved:?}'");

                resolved
            },
        };

        let flake_ref = match url.scheme() {
            _ if FileRef::<protocol::File>::parses(s) => {
                s.parse::<FileRef<protocol::File>>()?.into()
            },
            _ if FileRef::<protocol::HTTP>::parses(s) => {
                s.parse::<FileRef<protocol::HTTP>>()?.into()
            },
            _ if FileRef::<protocol::HTTPS>::parses(s) => {
                s.parse::<FileRef<protocol::HTTPS>>()?.into()
            },
            _ if TarballRef::<protocol::File>::parses(s) => {
                s.parse::<TarballRef<protocol::File>>()?.into()
            },
            _ if TarballRef::<protocol::HTTP>::parses(s) => {
                s.parse::<TarballRef<protocol::HTTP>>()?.into()
            },
            _ if TarballRef::<protocol::HTTPS>::parses(s) => {
                s.parse::<TarballRef<protocol::HTTPS>>()?.into()
            },
            _ if GitServiceRef::<service::Github>::parses(s) => {
                s.parse::<GitServiceRef<service::Github>>()?.into()
            },
            _ if GitServiceRef::<service::Gitlab>::parses(s) => {
                s.parse::<GitServiceRef<service::Gitlab>>()?.into()
            },
            _ if PathRef::parses(s) => s.parse::<PathRef>()?.into(),
            _ if GitRef::<protocol::File>::parses(s) => {
                dbg!(GitRef::<protocol::File>::from_url(dbg!(url))?.into())
            },
            _ if GitRef::<protocol::SSH>::parses(s) => s.parse::<GitRef<protocol::SSH>>()?.into(),
            _ if GitRef::<protocol::HTTP>::parses(s) => s.parse::<GitRef<protocol::HTTP>>()?.into(),
            _ if GitRef::<protocol::HTTPS>::parses(s) => {
                s.parse::<GitRef<protocol::HTTPS>>()?.into()
            },
            _ if IndirectRef::parses(s) => s.parse::<IndirectRef>()?.into(),
            _ => Err(ParseFlakeRefError::Invalid)?,
        };
        Ok(flake_ref)
    }
}

impl FlakeRef {
    /// Resolve an abbreviated URL to local files
    ///
    /// Nix supports referring to local files/paths without an explicit scheme.
    /// However, it distinguishes between flakes in git repositories
    /// (disambiguated using `git+file://`) from "free" ones (`path:`).
    ///
    /// The logic for this in nix can be found at <https://github.com/NixOS/nix/blob/bf7dc3c7dc24f75fa623135750e8f10b8bcd94f9/src/libexpr/flake/flakeref.cc#L112C5-L197>
    ///
    /// This method implements a subset of this logic assuming we require the path to exist and to be a flake.
    ///
    /// In abstract we
    ///
    /// 1. locate a flake (by finding a `flake.nix` in all ancestors)
    ///   - fail if directory does not exist
    ///   - fail if skipping file system boundaries
    ///   - fail if "exiting" a git repository (if there is one)
    /// 2. locate the root of the "containing" git repo (if applicable)
    ///   - check whether `.git/shallow` exists and track in the url params
    ///   - ensure there is no `?dir=` param if flake is in a subdir
    /// 3. construct a `file+git:` or `path:` url as required
    pub fn resolve_local(s: impl AsRef<str>) -> Result<Url, ResolveLocalRefError> {
        let s = s.as_ref();
        let mut git_url =
            Url::parse(&format!("git+file:{s}")).map_err(ResolveLocalRefError::ParseUrl)?;

        let path = Path::new(git_url.path());
        let path = path
            .canonicalize()
            .map_err(|err| ResolveLocalRefError::Canonicalize(path.to_path_buf(), err))?;

        // update base to canonicalized path
        git_url.set_path(&path.to_string_lossy());

        let original_device = path
            .metadata()
            .map_err(|err| ResolveLocalRefError::Metadata(path.to_path_buf(), err))?
            .dev();

        let mut ancestors = path.ancestors();

        let flake_root = loop {
            let ancestor = ancestors
                .next()
                .ok_or_else(|| ResolveLocalRefError::NotFound(path.to_path_buf()))?;

            debug!("looking for flake in {ancestor:?}");

            {
                let device = ancestor
                    .metadata()
                    .map_err(|err| ResolveLocalRefError::Metadata(path.to_path_buf(), err))?
                    .dev();

                if device != original_device {
                    Err(ResolveLocalRefError::FileSystemBoundary(
                        ancestor.to_path_buf(),
                    ))?
                }
            }

            if ancestor.join("flake.nix").exists() {
                debug!("found for flake in {ancestor:?}");
                break ancestor;
            }

            if ancestor.join(".git").exists() {
                Err(ResolveLocalRefError::GitRepoBoundary(
                    ancestor.to_path_buf(),
                ))?;
            }
        };

        // if there is git
        if let Some(git_root) = flake_root.ancestors().find(|ancestor| {
            debug!("looking for git repo in {ancestor:?}");

            ancestor.join(".git").exists()
        }) {
            let mut found = Url::parse("git+file:/").unwrap();
            found.set_query(git_url.query());
            found.set_path(&git_root.to_string_lossy());

            if git_root != flake_root {
                let dir_param = flake_root.strip_prefix(git_root).unwrap();

                found
                    .query_pairs_mut()
                    .append_pair("dir", &dir_param.to_string_lossy())
                    .finish();
            }

            if git_url
                .query_pairs()
                .collect::<HashMap<Cow<_>, Cow<_>>>()
                .contains_key("dir")
            {
                Err(ResolveLocalRefError::InconsistentDirParam(
                    git_url.to_string(),
                    found.to_string(),
                ))?;
            }

            if git_root.join(".git").join("shallow").exists() {
                found.query_pairs_mut().append_pair("shallow", "1").finish();
            }
            Ok(found)
        } else {
            debug!("no git repo found, resolving as 'path:'");
            let mut path_url = Url::parse("path:/").unwrap();
            path_url.set_path(&path.to_string_lossy());
            path_url.set_query(git_url.query());
            Ok(path_url)
        }
    }
}

#[derive(Debug, Error)]
pub enum ResolveLocalRefError {
    #[error(transparent)]
    ParseUrl(#[from] url::ParseError),

    #[error("Could not canonicalize path {0:?}: {1}")]
    Canonicalize(PathBuf, std::io::Error),

    #[error("Could not read metadata for dir: {0:?}: {1}")]
    Metadata(PathBuf, std::io::Error),

    #[error("Unable to find a flake before encountering file system boundary at {0:?}")]
    FileSystemBoundary(PathBuf),

    #[error("Unable to locate flake in git repo {0:?}")]
    GitRepoBoundary(PathBuf),

    #[error("could not find a flake in {0:?} or its parents")]
    NotFound(PathBuf),

    #[error("path {0:?} is not a flake (because it's not a directory)")]
    NotADirectory(PathBuf),

    #[error("flake URL '{0}' has an inconsistent 'dir' parameter, found flake in '{1} ")]
    InconsistentDirParam(String, String),
}

#[derive(Debug, Error)]
pub enum ParseFlakeRefError {
    #[error(transparent)]
    File(#[from] file::ParseFileError),
    #[error(transparent)]
    GitService(#[from] git_service::ParseGitServiceError),
    #[error(transparent)]
    Git(#[from] git::ParseGitError),
    #[error(transparent)]
    Indirect(#[from] indirect::ParseIndirectError),
    #[error(transparent)]
    Path(#[from] path::ParsePathRefError),
    #[error(transparent)]
    Local(#[from] ResolveLocalRefError),
    #[error("Invalid flakeref")]
    Invalid,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, From, Clone)]
#[serde(try_from = "TimestampDeserialize")]
pub struct Timestamp(
    #[serde(serialize_with = "chrono::serde::ts_seconds::serialize")] chrono::DateTime<chrono::Utc>,
);

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum TimestampDeserialize {
    TsI64(i64),
    TsString(String),
}

impl TryFrom<TimestampDeserialize> for Timestamp {
    type Error = ParseTimeError;

    fn try_from(value: TimestampDeserialize) -> Result<Self, Self::Error> {
        let ts = match value {
            TimestampDeserialize::TsI64(t) => Utc
                .timestamp_opt(t, 0)
                .earliest()
                .ok_or(ParseTimeError::FromInt(t))?,
            // per <https://docs.rs/chrono/0.4.24/chrono/format/strftime/index.html>
            TimestampDeserialize::TsString(s) => NaiveDateTime::parse_from_str(&s, "%s")?
                .and_local_timezone(Utc)
                .earliest()
                .unwrap(),
        };
        Ok(Timestamp(ts))
    }
}

#[derive(Debug, Error)]
pub enum ParseTimeError {
    #[error("Could not parse {0} to UTC date")]
    FromInt(i64),
    #[error(transparent)]
    FromString(#[from] chrono::ParseError),
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum BoolReprs {
    String(String),
    Bool(bool),
}

impl TryFrom<BoolReprs> for bool {
    type Error = <bool as FromStr>::Err;

    fn try_from(value: BoolReprs) -> Result<Self, Self::Error> {
        match value {
            BoolReprs::String(s) => s.parse::<bool>(),
            BoolReprs::Bool(b) => Ok(b),
        }
    }
}

impl BoolReprs {
    pub fn deserialize_bool<'de, D>(deserializer: D) -> Result<bool, D::Error>
    where
        D: Deserializer<'de>,
    {
        BoolReprs::deserialize(deserializer)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
pub(super) mod tests {

    pub(super) fn roundtrip_to<T>(input: &str, output: &str)
    where
        T: FromStr + Display,
        <T as FromStr>::Err: Debug + Display,
    {
        let parsed = input
            .parse::<T>()
            .unwrap_or_else(|e| panic!("'{input}' should parse: \n{e}\n{e:#?}"));
        assert_eq!(parsed.to_string(), output);
    }

    pub(super) fn roundtrip<T>(input: &str)
    where
        T: FromStr + Display,
        <T as FromStr>::Err: Debug + Display,
    {
        roundtrip_to::<T>(input, input)
    }

    use std::env;
    use std::fmt::Debug;
    use std::fs::{self, File};

    use super::*;

    #[test]
    fn test_all_parsing() {
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+file:///somewhere/there")).unwrap(),
            FlakeRef::FileFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file:///somewhere/there")).unwrap(),
            FlakeRef::FileFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+http://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("http://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+https://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTPS(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("https://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTPS(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+file:///somewhere/there")).unwrap(),
            FlakeRef::TarballFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file:///somewhere/there.tar.gz")).unwrap(),
            FlakeRef::TarballFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+http://my.de/path/to/file")).unwrap(),
            FlakeRef::TarballHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("http://my.de/path/to/file.tar.gz")).unwrap(),
            FlakeRef::TarballHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+https://my.de/path/to/file")).unwrap(),
            FlakeRef::TarballHTTPS(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("https://my.de/path/to/file.tar.gz")).unwrap(),
            FlakeRef::TarballHTTPS(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("github:flox/runix")).unwrap(),
            FlakeRef::Github(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("gitlab:flox/runix")).unwrap(),
            FlakeRef::Gitlab(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("path:/somewhere/there")).unwrap(),
            FlakeRef::Path(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("git+file:///somewhere/there")).unwrap(),
            FlakeRef::GitPath(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("git+ssh://github.com/flox/runix")).unwrap(),
            FlakeRef::GitSsh(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("git+https://github.com/flox/runix")).unwrap(),
            FlakeRef::GitHttps(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("git+http://github.com/flox/runix")).unwrap(),
            FlakeRef::GitHttp(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("flake:nixpkgs")).unwrap(),
            FlakeRef::Indirect(_)
        ));
    }

    #[test]
    fn test_resolve_absolute_non_git_local() {
        let flake_test_dir = tempfile::tempdir().unwrap();

        let basic = flake_test_dir.path().canonicalize().unwrap().join("basic");
        fs::create_dir_all(&basic).unwrap();
        File::create(basic.join("flake.nix")).unwrap();

        let basic = basic.to_string_lossy();

        assert_eq!(
            FlakeRef::resolve_local(&basic).unwrap().to_string(),
            Url::parse(&format!("path:{basic}")).unwrap().to_string()
        )
    }

    #[test]
    fn test_resolve_relative_non_git_local() {
        let flake_test_dir = tempfile::tempdir().unwrap();

        let basic = flake_test_dir.path().canonicalize().unwrap().join("basic");
        fs::create_dir_all(&basic).unwrap();
        File::create(basic.join("flake.nix")).unwrap();

        let path = pathdiff::diff_paths(&basic, env::current_dir().unwrap()).unwrap();

        assert_eq!(
            FlakeRef::resolve_local(&path.to_string_lossy())
                .unwrap()
                .to_string(),
            Url::parse(&format!("path:{}", basic.to_string_lossy()))
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_resolve_absolute_git_local() {
        let flake_test_dir = tempfile::tempdir().unwrap();

        let git_dir = flake_test_dir
            .path()
            .canonicalize()
            .unwrap()
            .join("withgit");
        fs::create_dir_all(&git_dir).unwrap();
        fs::create_dir_all(git_dir.join(".git")).unwrap();
        File::create(git_dir.join("flake.nix")).unwrap();

        let git_dir_string = git_dir.to_string_lossy();

        assert_eq!(
            FlakeRef::resolve_local(&git_dir_string)
                .unwrap()
                .to_string(),
            Url::parse(&format!("git+file:{git_dir_string}"))
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_resolve_absolute_git_local_nested() {
        let flake_test_dir = tempfile::tempdir().unwrap();

        let flake_dir_name = "inner";

        let git_dir = flake_test_dir
            .path()
            .canonicalize()
            .unwrap()
            .join("withgit");
        fs::create_dir_all(&git_dir).unwrap();
        fs::create_dir_all(git_dir.join(".git")).unwrap();

        let flake_root = git_dir.join(flake_dir_name);
        fs::create_dir_all(&flake_root).unwrap();

        File::create(flake_root.join("flake.nix")).unwrap();

        let git_dir_string = git_dir.to_string_lossy();

        assert_eq!(
            FlakeRef::resolve_local(&flake_root.to_string_lossy())
                .unwrap()
                .to_string(),
            Url::parse(&format!("git+file:{git_dir_string}?dir={flake_dir_name}"))
                .unwrap()
                .to_string()
        )
    }

    #[test]
    fn test_resolve_relative_git_local() {
        let flake_test_dir = tempfile::tempdir().unwrap();

        let git_dir = flake_test_dir
            .path()
            .canonicalize()
            .unwrap()
            .join("withgit");
        fs::create_dir_all(&git_dir).unwrap();
        fs::create_dir_all(git_dir.join(".git")).unwrap();
        File::create(git_dir.join("flake.nix")).unwrap();

        let relative_path = pathdiff::diff_paths(&git_dir, env::current_dir().unwrap()).unwrap();

        assert_eq!(
            FlakeRef::resolve_local(relative_path.to_string_lossy())
                .unwrap()
                .to_string(),
            Url::parse(&format!("git+file:{}", git_dir.to_string_lossy()))
                .unwrap()
                .to_string()
        )
    }
}
