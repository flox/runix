use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::DerivationPath;

fn default_true() -> bool {
    true
}

/// Narinfo stores information output by `nix path-info --json`
#[derive(Serialize, Deserialize, Clone)]
pub struct Narinfo {
    pub path: DerivationPath,
    // TODO remove this
    // https://github.com/NixOS/nix/pull/7924 made it into 2.15.0, but keep this
    // a bit longer to support older Nix versions
    #[serde(default = "default_true")]
    pub valid: bool,
    // TODO add other fields
    #[serde(flatten)]
    _other: HashMap<String, Value>,
}
