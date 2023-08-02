use std::path::PathBuf;

use serde::Serialize;
use serde_with::{serde_as, DisplayFromStr};

use crate::flake_ref::lock::{Rev, RevCount};
use crate::flake_ref::{self};

pub type FlakeLock = serde_json::Value;

/// Flake Metadata as it is exposed through `nix flake metadata`
#[serde_as]
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FlakeMetadata {
    pub description: Option<String>,
    pub last_modified: flake_ref::Timestamp,

    pub locks: FlakeLock,

    pub original: flake_ref::FlakeRef,
    pub locked: flake_ref::FlakeRef,

    #[serde_as(as = "DisplayFromStr")]
    pub original_url: flake_ref::FlakeRef,
    #[serde_as(as = "DisplayFromStr")]
    pub resolved_url: flake_ref::FlakeRef,
    #[serde_as(as = "DisplayFromStr")]
    pub url: flake_ref::FlakeRef,

    pub path: PathBuf,

    pub revision: Option<Rev>,
    pub rev_count: Option<RevCount>,
}
