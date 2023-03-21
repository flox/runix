use std::borrow::Cow;
use std::fmt::Display;
use std::path::Path;
use std::str::FromStr;

use derive_more::{Deref, Display};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use thiserror::Error;
use url::Url;

use super::lock::NarHash;
use super::protocol::{self, Protocol, WrappedUrl, WrappedUrlParseError};
use super::{BoolReprs, FlakeRefSource};

pub type FileUrl<Protocol> = WrappedUrl<Protocol>;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/tarball.cc#L287>
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq)]
#[serde(tag = "file")]
pub struct FileRef<Protocol: FileProtocol, A: ApplicationProtocol = File> {
    url: WrappedUrl<Protocol>,

    #[serde(skip)]
    _type: Application<A>,

    #[serde(flatten)]
    attributes: FileAttributes,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq)]
pub struct FileAttributes {
    #[serde(flatten)]
    nar_hash: Option<NarHash>,

    unpack: Option<Unpack>,

    name: Option<String>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Deref)]
struct Unpack(#[serde(deserialize_with = "BoolReprs::deserialize_bool")] bool);

pub trait ApplicationProtocol: Default {
    fn protocol() -> Cow<'static, str>;
    fn required(url: &Url) -> bool;
}

#[derive(Debug, Default, PartialEq, Eq, DeserializeFromStr, SerializeDisplay)]
pub struct Application<P: ApplicationProtocol> {
    inner: P,
}

#[derive(Debug, Error)]
#[error("Invalid Application '{0}', expected {1}")]
pub struct InvalidApplication<T: ApplicationProtocol>(String, Application<T>);

impl<T: ApplicationProtocol> FromStr for Application<T> {
    type Err = InvalidApplication<T>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s != T::protocol() {
            return Err(InvalidApplication(s.to_string(), Self::default()));
        }
        Ok(Self::default())
    }
}

impl<T: ApplicationProtocol> Display for Application<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", T::protocol())
    }
}

#[derive(Display, Default, Debug)]
pub struct File;
impl ApplicationProtocol for File {
    fn protocol() -> Cow<'static, str> {
        "file".into()
    }

    fn required(_url: &Url) -> bool {
        false
    }
}

#[derive(Display, Default, Debug)]
pub struct Tarball;
impl ApplicationProtocol for Tarball {
    fn protocol() -> Cow<'static, str> {
        "tarball".into()
    }

    fn required(url: &Url) -> bool {
        let is_tarball_url = Path::new(url.path())
            .extension()
            .map(|ext| {
                [
                    ".zip", ".tar", ".tgz", ".tar.gz", ".tar.xz", ".tar.bz2", ".tar.zst",
                ]
                .contains(&&*ext.to_string_lossy())
            })
            .unwrap_or_default();

        !is_tarball_url
    }
}

pub trait FileProtocol: Protocol {}
impl FileProtocol for protocol::File {}
impl FileProtocol for protocol::HTTP {}
impl FileProtocol for protocol::HTTPS {}

impl<Protocol: FileProtocol, Type: ApplicationProtocol> FlakeRefSource for FileRef<Protocol, Type> {
    fn scheme() -> Cow<'static, str> {
        format!(
            "{outer}+{inner}:",
            outer = Type::protocol(),
            inner = Protocol::scheme()
        )
        .into()
    }

    fn parses(maybe_ref: &str) -> bool {
        if maybe_ref.starts_with(&*Self::scheme()) {
            return true;
        }

        if !Url::parse(maybe_ref)
            .map(|url| Type::required(&url))
            .unwrap_or_default()
        {
            return maybe_ref.starts_with(&format!("{scheme}:", scheme = Protocol::scheme()));
        }

        false
    }
}

impl<Protocol: FileProtocol, Type: ApplicationProtocol> Display for FileRef<Protocol, Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{outer}+{url}{attributes}",
            outer = self._type,
            url = self.url,
            attributes = serde_urlencoded::to_string(&self.attributes).unwrap_or_default()
        )
    }
}

impl<Protocol: FileProtocol, App: ApplicationProtocol> FromStr for FileRef<Protocol, App> {
    type Err = ParseFileError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;

        // fix url
        let (app, proto, url, wrapped) = if let Some((app, proto)) = &url.scheme().split_once('+') {
            let wrapped = FileUrl::<Protocol>::from_str(s.trim_start_matches(&format!("{app}+")))?;

            (app.to_string(), proto.to_string(), url, wrapped)
        } else if App::required(&url) {
            return Err(ParseFileError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        } else {
            let app = App::protocol();
            let proto = url.scheme();
            let fixed = Url::parse(&format!("{app}+{url}"))?;
            (
                app.to_string(),
                proto.to_string(),
                fixed,
                FileUrl::<Protocol>::from_str(s)?,
            )
        };

        if app != App::protocol() || proto != Protocol::scheme() {
            return Err(ParseFileError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        };

        let attributes: FileAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or_default())?;

        Ok(FileRef {
            url: wrapped,
            attributes,
            _type: Application::default(),
        })
    }
}

#[derive(Debug, Error)]
pub enum ParseFileError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error(transparent)]
    FileUrl(#[from] WrappedUrlParseError),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:'")]
    InvalidScheme(String, String),
    #[error("No repo specified")]
    NoRepo,
    #[error("Couldn't parse query")]
    Query(#[from] serde_urlencoded::de::Error),
}

#[cfg(test)]
mod tests {}
