use std::borrow::Cow;
use std::fmt::Display;
use std::str::FromStr;

use chrono::{NaiveDateTime, TimeZone, Utc};
use derive_more::{Display, From};
use serde::{Deserialize, Deserializer, Serialize};
use thiserror::Error;

use self::file::FileRef;
use self::git::GitRef;
use self::git_service::{service, GitServiceSource};
use self::indirect::IndirectRef;
use self::path::PathRef;

pub mod file;
pub mod git;
pub mod git_service;
pub mod indirect;
pub mod lock;
pub mod path;
pub mod protocol;

pub trait FlakeRefSource: FromStr + Display {
    fn scheme() -> Cow<'static, str>;
    fn parses(maybe_ref: &str) -> bool {
        maybe_ref.starts_with(&format!("{}:", Self::scheme()))
    }
}

#[derive(Serialize, Deserialize, Display, From, Debug)]
#[serde(untagged)]
pub enum FlakeRef {
    FileFile(FileRef<protocol::File, file::File>),
    FileHTTP(FileRef<protocol::HTTP, file::File>),
    FileHTTPS(FileRef<protocol::HTTPS, file::File>),
    TarballFile(FileRef<protocol::File, file::Tarball>),
    TarballHTTP(FileRef<protocol::HTTP, file::Tarball>),
    TarballHTTPS(FileRef<protocol::HTTPS, file::Tarball>),
    Github(GitServiceSource<service::Github>),
    Gitlab(GitServiceSource<service::Gitlab>),
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let flake_ref = match s {
            _ if FileRef::<protocol::File>::parses(s) => {
                s.parse::<FileRef<protocol::File>>()?.into()
            },
            _ if FileRef::<protocol::HTTP>::parses(s) => {
                s.parse::<FileRef<protocol::HTTP>>()?.into()
            },
            _ if FileRef::<protocol::HTTPS>::parses(s) => {
                s.parse::<FileRef<protocol::HTTPS>>()?.into()
            },
            _ if FileRef::<protocol::File, file::Tarball>::parses(s) => {
                s.parse::<FileRef<protocol::File, file::Tarball>>()?.into()
            },
            _ if FileRef::<protocol::HTTP, file::Tarball>::parses(s) => {
                s.parse::<FileRef<protocol::HTTP, file::Tarball>>()?.into()
            },
            _ if FileRef::<protocol::HTTPS, file::Tarball>::parses(s) => {
                s.parse::<FileRef<protocol::HTTPS, file::Tarball>>()?.into()
            },
            _ if GitServiceSource::<service::Github>::parses(s) => {
                s.parse::<GitServiceSource<service::Github>>()?.into()
            },
            _ if GitServiceSource::<service::Gitlab>::parses(s) => {
                s.parse::<GitServiceSource<service::Gitlab>>()?.into()
            },
            _ if PathRef::parses(s) => s.parse::<PathRef>()?.into(),
            _ if GitRef::<protocol::File>::parses(s) => s.parse::<GitRef<protocol::File>>()?.into(),
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

    #[error("Invalid flakeref")]
    Invalid,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, From)]
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
mod tests {
    use super::*;

    #[test]
    fn test_all_parsing() {
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+file:///somewhere/there")).unwrap(),
            FlakeRef::FileFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+http://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("file+https://my.de/path/to/file")).unwrap(),
            FlakeRef::FileHTTPS(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+file:///somewhere/there")).unwrap(),
            FlakeRef::TarballFile(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+http://my.de/path/to/file")).unwrap(),
            FlakeRef::TarballHTTP(_)
        ));
        assert!(matches!(
            dbg!(FlakeRef::from_str("tarball+https://my.de/path/to/file")).unwrap(),
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
}
