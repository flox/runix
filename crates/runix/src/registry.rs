//! A rust implementaiton of the `registry` file format

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use serde_with::skip_serializing_none;
use thiserror::Error;

use crate::flake_ref::indirect::IndirectRef;
use crate::flake_ref::FlakeRef;

#[derive(Error, Debug)]
pub enum RegistryError {}

#[derive(Debug, Clone, Deserialize, Serialize, Default, PartialEq, Eq)]
pub struct Registry {
    version: Version,
    /// Uses BTree implmentation to guarantee stable outputs
    /// [BTreeSet] unlike [std::collections::HashSet] guarantees
    /// that reading the set from a file and writing it back unchanged
    /// won't change the order of the elements.
    /// Hash Sets employ stochastic methods, that may change this order
    /// at the benefit of O(1) access (rather than O(log n) with BTree)
    flakes: BTreeSet<RegistryEntry>,
}

impl Registry {
    pub fn set(&mut self, name: impl ToString, to: FlakeRef) {
        let entry = RegistryEntry {
            from: IndirectRef {
                _type: Default::default(),
                id: name.to_string(),
                attributes: Default::default(),
            },
            to,
            exact: None,
        };
        self.flakes.replace(entry);
    }

    #[allow(unused)]
    /// Todo: more functions such as remove, get, etc
    pub fn remove(&mut self, _name: impl ToString) {}

    /// Iterate over the entries in the registry
    pub fn entries(&self) -> impl Iterator<Item = &RegistryEntry> {
        self.flakes.iter()
    }
}

/// TODO: use https://github.com/dtolnay/serde-repr?
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
struct Version(u8);
impl Default for Version {
    fn default() -> Self {
        Self(2)
    }
}

#[skip_serializing_none]
#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
pub struct RegistryEntry {
    pub from: IndirectRef, // TODO merge into single flakeRef type @notgne2?
    pub to: FlakeRef,
    pub exact: Option<bool>,
}

impl Ord for RegistryEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.from.cmp(&other.from)
    }
}

impl PartialOrd for RegistryEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.from.partial_cmp(&other.from)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
enum FromFlakeRef {
    Indirect(IndirectRef),
}

#[cfg(test)]
mod tests {
    use std::fs::File;

    use super::*;
    #[test]
    fn parses_nix_registry() {
        serde_json::from_reader::<_, Registry>(File::open("./test/registry.test.json").unwrap())
            .expect("should parse");
    }
}
