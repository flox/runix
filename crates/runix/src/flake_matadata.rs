use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use serde_with::serde_as;

use crate::flake_ref::lock::{Rev, RevCount};
use crate::flake_ref::{self};

pub type FlakeLock = serde_json::Value;

/// Flake Metadata as it is exposed through `nix flake metadata`
#[serde_as]
#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct FlakeMetadata {
    description: String,
    last_modified: flake_ref::Timestamp,

    locks: FlakeLock,

    original: flake_ref::FlakeRef,
    locked: flake_ref::FlakeRef,

    #[serde_with(as = "DisplayFromStr")]
    original_url: flake_ref::FlakeRef,
    #[serde_with(as = "DisplayFromStr")]
    locked_url: flake_ref::FlakeRef,
    #[serde_with(as = "DisplayFromStr")]
    url: flake_ref::FlakeRef,

    path: PathBuf,

    revision: Option<Rev>,
    rev_count: Option<RevCount>,
}
