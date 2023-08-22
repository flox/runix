//! Command's own arguments, Option groups and [InstallableArg]s

use std::path::PathBuf;

use derive_more::{Deref, From};
use runix_derive::ToArgs;

use self::common::NixCommonArgs;
use self::config::NixConfigArgs;
use crate::command_line::ToArgs;
use crate::default::flag::{Flag, FlagType};
use crate::installable::{FlakeAttribute, Installable};

pub mod common;
pub mod config;
pub mod eval;
pub mod flake;
pub mod source;

/// Nix arguments
/// should be a proper struct + de/serialization to and from [&str]
#[derive(Debug, Default)]
pub struct NixArgs {
    /// Configure the cwd for nix actions.
    ///
    /// Relevant for instance for init and relative installables
    pub cwd: Option<PathBuf>,

    /// Common arguments to the nix command
    pub common: NixCommonArgs,

    /// Nix configuration (overrides nix.conf)
    pub config: NixConfigArgs,
}

impl ToArgs for NixArgs {
    fn to_args(&self) -> Vec<String> {
        [self.config.to_args(), self.common.to_args()]
            .into_iter()
            .flatten()
            .collect()
    }
}

/// Installable argument for commands taking a single Installable
/// ([approximately](https://github.com/NixOS/nix/search?q=InstallablesCommand)
#[derive(From, Clone, Default, Debug)]
#[from(forward)]
pub struct InstallableArg(Option<Installable>);
impl ToArgs for InstallableArg {
    fn to_args(&self) -> Vec<String> {
        self.0.iter().map(|i| i.to_string()).collect()
    }
}

impl From<FlakeAttribute> for InstallableArg {
    fn from(flake_attribute: FlakeAttribute) -> Self {
        Self(Some(flake_attribute.into()))
    }
}

/// Installable argument for commands taking multiple Installables
/// ([approximately](https://github.com/NixOS/nix/search?q=InstallablesCommand)
#[derive(Debug, From, Default, Clone)]
#[from(forward)]
pub struct InstallablesArgs(Vec<Installable>);
impl ToArgs for InstallablesArgs {
    fn to_args(&self) -> Vec<String> {
        self.0.iter().map(|i| i.to_string()).collect()
    }
}

/// `nix --out-path <path>` option
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct OutLink(PathBuf);
impl Flag for OutLink {
    const FLAG: &'static str = "--out-link";
    const FLAG_TYPE: FlagType<Self> = FlagType::os_str_arg();
}

/// `nix build --no-link` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct NoLink(bool);
impl Flag for NoLink {
    const FLAG: &'static str = "--no-link";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix build` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct BuildArgs {
    pub out_link: Option<OutLink>,
    pub no_link: Option<NoLink>,
}

/// `nix develop` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct DevelopArgs {}

/// `nix bundle --bundler <bundler>` option
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct Bundler(Installable);
impl Flag for Bundler {
    const FLAG: &'static str = "--bundler";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

/// `nix bundle` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct BundleArgs {
    pub bundler: Option<Bundler>,
}

/// `nix eval --apply <expr>` option
#[derive(Clone, From, Deref, Debug, Default)]
#[from(forward)]
pub struct Apply(String);
impl Flag for Apply {
    const FLAG: &'static str = "--apply";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

/// [`nix eval`](https://github.com/NixOS/nix/blob/a6239eb5700ebb85b47bb5f12366404448361f8d/src/nix/eval.cc#LL21-40) options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct EvalArgs {
    pub apply: Option<Apply>,
    pub installable: Option<InstallableArg>,
}

/// `nix store gc --dry-run` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct DryRun(bool);
impl Flag for DryRun {
    const FLAG: &'static str = "--dry-run";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix store gc --max <n>` option
#[derive(Clone, From, Deref, Debug, Default)]
#[from(forward)]
pub struct Max(u32);
impl Flag for Max {
    const FLAG: &'static str = "--max";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

/// `nix store gc` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct StoreGcArgs {
    pub dry_run: Option<DryRun>,
    pub max: Option<Max>,
}

/// `nix copy` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct CopyArgs {
    // TODO --no-check-sigs, --substitute-on-destination
    pub from: Option<CopyFrom>,
    pub to: Option<CopyTo>,
}

/// `nix copy --from` option
#[derive(Debug, Clone, Deref, Default, From)]
#[from(forward)]
pub struct CopyFrom(String);
impl Flag for CopyFrom {
    const FLAG: &'static str = "--from";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

/// `nix copy --to` option
#[derive(Debug, Clone, Deref, Default, From)]
#[from(forward)]
pub struct CopyTo(String);
impl Flag for CopyTo {
    const FLAG: &'static str = "--to";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

/// `nix path-info --closure-size` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct ClosureSize(bool);
impl Flag for ClosureSize {
    const FLAG: &'static str = "--closure-size";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix path-info --human-readable` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct HumanReadable(bool);
impl Flag for HumanReadable {
    const FLAG: &'static str = "--human-readable";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix path-info --sigs` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct Sigs(bool);
impl Flag for Sigs {
    const FLAG: &'static str = "--sigs";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix path-info --size` flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct Size(bool);
impl Flag for Size {
    const FLAG: &'static str = "--size";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix path-info` options
#[derive(Debug, Default, Clone, ToArgs)]
pub struct PathInfoArgs {
    pub closure_size: Option<ClosureSize>,
    pub human_readable: Option<HumanReadable>,
    pub sigs: Option<Sigs>,
    pub size: Option<Size>,
}

/// `nix store sign --recursive` flag
///
/// Technically an extended installable flag
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct Recursive(bool);
impl Flag for Recursive {
    const FLAG: &'static str = "--recursive";
    const FLAG_TYPE: FlagType<Self> = FlagType::switch(false);
}

/// `nix store sign --key-file <FILE>` option
#[derive(Clone, From, Deref, Debug)]
#[from(forward)]
pub struct KeyFile(PathBuf);
impl Flag for KeyFile {
    const FLAG: &'static str = "--key-file";
    const FLAG_TYPE: FlagType<Self> = FlagType::os_str_arg();
}

/// `nix store sign` options
#[derive(Debug, Clone, ToArgs)]
pub struct StoreSignArgs {
    pub key_file: KeyFile,
    pub recursive: Option<Recursive>,
}
