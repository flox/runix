use std::str::FromStr;

use derive_more::{Deref, From};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_with::DeserializeFromStr;
use thiserror::Error;

use super::Timestamp;

static HASH_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("[a-f0-9]{40}").unwrap());

#[derive(Debug, Deserialize, Serialize, Clone, Deref, PartialEq, Eq, From)]
#[from(forward)]
pub struct NarHash {
    #[serde(rename = "narHash")]
    nar_hash: String,
}

#[derive(Debug, Deserialize, Serialize, Deref, PartialEq, Eq, From)]
#[from(forward)]
pub struct LastModified {
    #[serde(rename = "lastModified")]
    last_modified: Timestamp,
}

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
