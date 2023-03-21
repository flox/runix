use std::borrow::Cow;
use std::marker::PhantomData;
use std::str::FromStr;

use derive_more::{Deref, Display};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};
use thiserror::Error;
use url::Url;

pub trait Protocol {
    fn scheme() -> Cow<'static, str>;
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
pub struct File;
impl Protocol for File {
    fn scheme() -> Cow<'static, str> {
        "file".into()
    }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
pub struct SSH;
impl Protocol for SSH {
    fn scheme() -> Cow<'static, str> {
        "ssh".into()
    }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
pub struct HTTPS;
impl Protocol for HTTPS {
    fn scheme() -> Cow<'static, str> {
        "https".into()
    }
}

#[derive(Serialize, Deserialize, Default, Debug, PartialEq, Eq)]
pub struct HTTP;
impl Protocol for HTTP {
    fn scheme() -> Cow<'static, str> {
        "http".into()
    }
}

#[derive(DeserializeFromStr, SerializeDisplay, Display, Debug, PartialEq, Eq, Deref)]
#[display(fmt = "{inner}")]
pub struct WrappedUrl<Protocol> {
    #[deref]
    inner: Url,
    _protocol: PhantomData<Protocol>,
}

impl<P: Protocol> TryFrom<Url> for WrappedUrl<P> {
    type Error = WrappedUrlParseError;

    fn try_from(mut url: Url) -> Result<Self, Self::Error> {
        if url.scheme() != P::scheme() {
            return Err(WrappedUrlParseError::Protocol(url.scheme().to_string()));
        }
        url.set_fragment(None);
        url.set_query(None);
        Ok(WrappedUrl {
            inner: url,
            _protocol: PhantomData,
        })
    }
}

impl<P: Protocol> FromStr for WrappedUrl<P> {
    type Err = WrappedUrlParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Url::parse(s)?.try_into()
    }
}

#[derive(Debug, Error)]
pub enum WrappedUrlParseError {
    #[error(transparent)]
    Url(#[from] url::ParseError),
    #[error("Unsupported protocol: {0}")]
    Protocol(String),
}
