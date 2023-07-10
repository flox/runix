use std::str::FromStr;

use derive_more::Deref;
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_with::DeserializeFromStr;
use thiserror::Error;

use super::Timestamp;

static HASH_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("[a-f0-9]{40}").unwrap());

/// todo: parse/validate narHash?
pub type NarHash = String;
pub type LastModified = Timestamp;

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
#[serde(untagged)]
pub enum RevOrRef {
    Rev {
        rev: Rev,
    },
    Ref {
        #[serde(rename = "ref")]
        reference: String,
    },
}

impl From<String> for RevOrRef {
    fn from(value: String) -> Self {
        Rev::from_str(&value)
            .map(|rev| RevOrRef::Rev { rev })
            .unwrap_or(RevOrRef::Ref { reference: value })
    }
}

#[derive(DeserializeFromStr, Serialize, Clone, Debug, PartialEq, Eq, Deref)]
pub struct Rev(String);
impl FromStr for Rev {
    type Err = InvalidRev;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !HASH_REGEX.is_match(s) {
            Err(InvalidRev)
        } else {
            Ok(Rev(s.to_string()))
        }
    }
}

#[derive(Error, Debug)]
#[error("Invalid revision hash")]
pub struct InvalidRev;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize, Clone)]
#[serde(try_from = "StringOrInt")]
pub struct RevCount(u64);

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum StringOrInt {
    String(String),
    Int(u64),
}

impl TryFrom<StringOrInt> for RevCount {
    type Error = <u64 as FromStr>::Err;

    fn try_from(value: StringOrInt) -> Result<Self, Self::Error> {
        match value {
            StringOrInt::String(s) => Ok(RevCount(s.parse()?)),
            StringOrInt::Int(i) => Ok(RevCount(i)),
        }
    }
}
