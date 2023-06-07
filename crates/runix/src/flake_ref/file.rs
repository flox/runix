use std::borrow::Cow;
use std::fmt::Display;
use std::str::FromStr;

use derive_more::Deref;
use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;
use url::Url;

use self::application::{Application, ApplicationProtocol};
use super::lock::NarHash;
use super::protocol::{self, Protocol, WrappedUrl, WrappedUrlParseError};
use super::{BoolReprs, FlakeRefSource};

pub type FileUrl<Protocol> = WrappedUrl<Protocol>;

/// <https://cs.github.com/NixOS/nix/blob/f225f4307662fe9a57543d0c86c28aa9fddaf0d2/src/libfetchers/tarball.cc#L287>
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone)]
#[serde(deny_unknown_fields)]
pub struct FileBasedRef<Protocol: FileProtocol, A: ApplicationProtocol> {
    pub url: WrappedUrl<Protocol>,

    #[serde(rename = "type")]
    #[serde(bound(deserialize = "Application<A>: Deserialize<'de>"))]
    #[serde(bound(serialize = "Application<A>: Serialize"))]
    _type: Application<A>,

    #[serde(flatten)]
    pub attributes: FileAttributes,
}

pub type FileRef<Protocol> = FileBasedRef<Protocol, application::File>;
pub type TarballRef<Protocol> = FileBasedRef<Protocol, application::Tarball>;

#[skip_serializing_none]
#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Clone, Default)]
#[serde(deny_unknown_fields)]
pub struct FileAttributes {
    #[serde(rename = "narHash")]
    pub nar_hash: Option<NarHash>,

    pub unpack: Option<Unpack>,

    pub name: Option<String>,
}

#[derive(Deserialize, Serialize, Debug, PartialEq, Eq, Deref, Clone)]
pub struct Unpack(#[serde(deserialize_with = "BoolReprs::deserialize_bool")] bool);

pub mod application {
    use std::borrow::Cow;
    use std::fmt::Display;
    use std::path::Path;
    use std::str::FromStr;

    use serde_with::{DeserializeFromStr, SerializeDisplay};
    use thiserror::Error;
    use url::Url;

    pub trait ApplicationProtocol: Default {
        /// Describes the the application name, ie.
        ///
        /// Applications are prepended to a url as `<application>+<url>`,
        /// to denote how the url should be parsed.
        ///
        /// ```
        /// # use runix::flake_ref::file::application::{ApplicationProtocol, File, Tarball};
        ///
        /// assert_eq!(Tarball::protocol(), "tarball");
        /// assert_eq!(File::protocol(), "file");
        /// ```
        fn protocol() -> Cow<'static, str>;

        /// Determines whether the application has to be provided for a given [Url]
        /// or can be implied.
        ///
        /// # Example
        ///
        /// The url <https://github.com/flox/runix/archive/refs/heads/main.tar.gz>
        /// implies a [Tarball].
        /// This it is not required to specify the application with `tarball+`.
        /// If the url is supoosed to be parsed as a [File] instead,
        /// the `file+` application must be added.
        ///
        /// ```
        /// use runix::flake_ref::file::application::{ApplicationProtocol, File, Tarball};
        /// use url::Url;
        /// let url = Url::parse("https://github.com/flox/runix/archive/refs/heads/main.tar.gz").unwrap();
        ///
        /// assert!(!Tarball::required(&url)); // application not required to parse [Tarball]
        /// assert!(File::required(&url)); // application required to parse as [File]
        /// ```
        fn required(url: &Url) -> bool;
    }

    #[derive(Debug, Default, PartialEq, Eq, DeserializeFromStr, SerializeDisplay, Clone)]
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

    fn is_tarball_url(url: &Url) -> bool {
        let is_tarball_url = Path::new(url.path())
            .file_name()
            .map(|name| {
                [
                    ".zip", ".tar", ".tgz", ".tar.gz", ".tar.xz", ".tar.bz2", ".tar.zst",
                ]
                .iter()
                .any(|ext| name.to_string_lossy().ends_with(ext))
            })
            .unwrap_or(false);

        is_tarball_url
    }

    #[derive(Default, Debug, Clone, PartialEq, Eq)]
    pub struct File;
    impl ApplicationProtocol for File {
        fn protocol() -> Cow<'static, str> {
            "file".into()
        }

        fn required(url: &Url) -> bool {
            is_tarball_url(url)
        }
    }

    #[derive(Default, Debug, Clone, PartialEq, Eq)]
    pub struct Tarball;
    impl ApplicationProtocol for Tarball {
        fn protocol() -> Cow<'static, str> {
            "tarball".into()
        }

        fn required(url: &Url) -> bool {
            !is_tarball_url(url)
        }
    }
}

pub trait FileProtocol: Protocol {}
impl FileProtocol for protocol::File {}
impl FileProtocol for protocol::HTTP {}
impl FileProtocol for protocol::HTTPS {}

impl<Protocol: FileProtocol, App: ApplicationProtocol> FlakeRefSource
    for FileBasedRef<Protocol, App>
{
    type ParseErr = ParseFileError;

    fn scheme() -> Cow<'static, str> {
        format!(
            "{outer}+{inner}",
            outer = App::protocol(),
            inner = Protocol::scheme()
        )
        .into()
    }

    /// A [FileBasedRef] has to hold three properties:
    ///
    /// 1. It has to be a [Url],
    ///    we cannot deduce a file from just a path (that would be a [PathRef])
    /// 2. the schema has to be a supported file protocol, i.e. one of
    ///    [`file://`, `http://`, `https://`]
    /// 3. the schema may contain an application, a hint as what the file should be parsed
    ///    Nix supports tarballs as their own filetype.
    ///    In some cases this application can be deduced from the url,
    ///    e.g. by looking at the path and file extension. (See [ApplicationProtocol::required])
    fn parses(maybe_ref: &str) -> bool {
        let url = if let Ok(url) = Url::parse(maybe_ref) {
            url
        } else {
            return false;
        };

        if url.scheme() == Self::scheme() {
            return true;
        }

        if !App::required(&url) && url.scheme() == Protocol::scheme() {
            return true;
        }

        false
    }

    fn from_url(url: Url) -> Result<Self, Self::ParseErr> {
        // resolve the application part from the url schema.
        // report missing application if required ( see [ApplicationProtocol::required])
        // deduce from url
        // - `app` will be the (implied) application
        // - `proto` the transport protocol from the url scheme
        // - `url` the url with application prepended
        // - `wrapped` the parsed url with guaranteed scheme
        let (app, proto, url, wrapped) = if let Some((app, proto)) = &url.scheme().split_once('+') {
            let wrapped = url
                .to_string()
                .trim_start_matches(&format!("{app}+"))
                .parse()?;

            (app.to_string(), proto.to_string(), url, wrapped)
        } else if !App::required(&url) {
            let app = App::protocol();
            let proto = url.scheme();
            let fixed = Url::parse(&format!("{app}+{url}"))?;
            (app.to_string(), proto.to_string(), fixed, url.try_into()?)
        } else {
            return Err(ParseFileError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        };

        if app != App::protocol() || proto != Protocol::scheme() {
            return Err(ParseFileError::InvalidScheme(
                Self::scheme().to_string(),
                url.scheme().to_string(),
            ));
        };

        let attributes: FileAttributes =
            serde_urlencoded::from_str(url.query().unwrap_or_default())?;

        Ok(FileBasedRef {
            url: wrapped,
            attributes,
            _type: Application::default(),
        })
    }
}

impl<Protocol: FileProtocol, App: ApplicationProtocol> FileBasedRef<Protocol, App> {
    pub fn new(url: WrappedUrl<Protocol>, attributes: FileAttributes) -> Self {
        Self {
            url,
            attributes,
            _type: Default::default(),
        }
    }
}

impl<Protocol: FileProtocol, App: ApplicationProtocol> Display for FileBasedRef<Protocol, App> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut url = if App::required(&self.url) {
            Url::parse(&format!(
                "{application}+{url}",
                application = self._type,
                url = self.url
            ))
            .unwrap()
        } else {
            self.url.clone()
        };

        url.set_query(
            serde_urlencoded::to_string(&self.attributes)
                .ok()
                .filter(|s| !s.is_empty())
                .as_deref(),
        );

        write!(f, "{url}")
    }
}

impl<Protocol: FileProtocol, App: ApplicationProtocol> FromStr for FileBasedRef<Protocol, App> {
    type Err = <Self as FlakeRefSource>::ParseErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let url = Url::parse(s)?;
        Self::from_url(url)
    }
}

#[derive(Debug, Error)]
pub enum ParseFileError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error(transparent)]
    FileUrl(#[from] WrappedUrlParseError),
    #[error("Invalid scheme (expected: '{0}:', found '{1}:')")]
    InvalidScheme(String, String),
    #[error("No repo specified")]
    NoRepo,
    #[error("Couldn't parse query: {0}")]
    Query(#[from] serde_urlencoded::de::Error),
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::flake_ref::tests::{roundtrip, roundtrip_to};

    type FileFileRef = super::FileRef<protocol::File>;
    type HttpFileRef = super::FileRef<protocol::HTTP>;
    type HttpsFileRef = super::FileRef<protocol::HTTPS>;

    type FileTarballRef = super::TarballRef<protocol::File>;
    type HttpTarballRef = super::TarballRef<protocol::HTTP>;
    type HttpsTarballRef = super::TarballRef<protocol::HTTPS>;

    #[test]
    fn file_file_roundtrips() {
        roundtrip::<FileFileRef>("file:///somewhere/there");
        roundtrip_to::<FileFileRef>("file+file:///somewhere/there", "file:///somewhere/there");
        roundtrip::<FileFileRef>("file:///somewhere/there?unpack=true");
    }

    #[test]
    fn file_http_roundtrips() {
        roundtrip::<HttpFileRef>("http://somewhere/there");
        roundtrip_to::<HttpFileRef>("file+http://somewhere/there", "http://somewhere/there");
        roundtrip::<HttpFileRef>("http://somewhere/there?unpack=true");
    }

    #[test]
    fn file_https_roundtrips() {
        roundtrip::<HttpsFileRef>("https://somewhere/there");
        roundtrip_to::<HttpsFileRef>("file+https://somewhere/there", "https://somewhere/there");
        roundtrip::<HttpsFileRef>("https://somewhere/there?unpack=true");
    }

    #[test]
    fn tarball_file_roundtrips() {
        roundtrip::<FileTarballRef>("tarball+file:///somewhere/there");
        roundtrip_to::<FileTarballRef>(
            "tarball+file:///somewhere/there.tar.gz",
            "file:///somewhere/there.tar.gz",
        );
        roundtrip::<FileTarballRef>("tarball+file:///somewhere/there?unpack=true");
    }

    #[test]
    fn tarball_http_roundtrips() {
        roundtrip::<HttpTarballRef>("tarball+http://somewhere/there");
        roundtrip_to::<HttpTarballRef>(
            "tarball+http://somewhere/there.tar.gz",
            "http://somewhere/there.tar.gz",
        );
        roundtrip::<HttpTarballRef>("tarball+http://somewhere/there?unpack=true");
    }

    #[test]
    fn tarball_https_roundtrips() {
        roundtrip::<HttpsTarballRef>("tarball+https://somewhere/there");
        roundtrip_to::<HttpsTarballRef>(
            "tarball+https://somewhere/there.tar.gz",
            "https://somewhere/there.tar.gz",
        );
        roundtrip::<HttpsTarballRef>("tarball+https://somewhere/there?unpack=true");
    }

    #[test]
    fn test_parse_nar_hash() {
        roundtrip::<FileFileRef>("file:///somewhere/there?narHash=sha256-MjeRjunqfGTBGU401nxIjs7PC9PZZ1FBCZp%2FbRB3C2M%3D")
    }
}
