//! Backened independent Command implementations

use std::collections::HashMap;

use derive_more::{Deref, From};
use serde::Deserialize;

use crate::arguments::eval::EvaluationArgs;
use crate::arguments::flake::FlakeArgs;
use crate::arguments::source::SourceArgs;
use crate::arguments::{BundleArgs, DevelopArgs, EvalArgs, InstallableArg, InstallablesArgs};
use crate::command_line::flag::{Flag, FlagType};
use crate::command_line::{Group, JsonCommand, NixCliCommand, TypedCommand};
use crate::installable::Installable;
use crate::flake_ref::ToFlakeRef;

/// `nix build` Command
#[derive(Debug, Default, Clone)]
pub struct Build {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub installables: InstallablesArgs,
}

impl NixCliCommand for Build {
    type Own = ();

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLES: Group<Self, InstallablesArgs> = Some(|d| d.installables.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["build"];
}
impl JsonCommand for Build {}
#[derive(Deserialize, Clone, Debug)]

/// Type for an element in the output of `nix build --json`
pub struct BuildOutEntry {
    #[serde(rename = "drvPath")]
    pub drv_path: String,
    pub outputs: HashMap<String, String>,
}

/// The output of `nix build --json`
pub type BuildOut = Vec<BuildOutEntry>;
impl TypedCommand for Build {
    type Output = BuildOut;
}

/// `nix flake init` Command
#[derive(Debug, Default, Clone)]
pub struct FlakeInit {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub installables: InstallablesArgs,

    pub template: Option<TemplateFlag>,
}

/// `nix flake init --template <TEMPLATE>` flag
#[derive(Deref, Debug, Clone, From)]
#[from(forward)]
pub struct TemplateFlag(Installable);
impl Flag for TemplateFlag {
    const FLAG: &'static str = "--template";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}

impl NixCliCommand for FlakeInit {
    type Own = Option<TemplateFlag>;

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLES: Group<Self, InstallablesArgs> = Some(|d| d.installables.clone());
    const OWN_ARGS: Group<Self, Option<TemplateFlag>> = Some(|d| d.template.clone());
    const SUBCOMMAND: &'static [&'static str] = &["flake", "init"];
}

/// `nix flake metadata` Command
#[derive(Debug, Default, Clone)]
pub struct FlakeMetadata {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub installable: InstallableArg,
}

impl NixCliCommand for FlakeMetadata {
    type Own = ();
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const INSTALLABLE: Group<Self, InstallableArg> = Some(|d| d.installable.clone());
    const SUBCOMMAND: &'static [&'static str] = &["flake", "metadata"];
}

impl JsonCommand for FlakeMetadata {}

#[derive(Deserialize, Clone, Debug)]
pub struct LockEntry {
    #[serde(default)]
    pub inputs: HashMap<String, String>,
    #[serde(default)]
    pub locked: Option<ToFlakeRef>,
    #[serde(default)]
    pub original: Option<ToFlakeRef>,
}

#[derive(Deserialize, Clone, Debug)]
pub struct Locks {
    pub root: String,
    pub version: u16,
    pub nodes: HashMap<String, LockEntry>,
}

#[derive(Deserialize, Clone, Debug)]
pub struct FlakeMetadataOut {
    #[serde(rename = "lastModified")]
    pub last_modified: u64,
    pub locked: ToFlakeRef,
    pub locks: Locks,
    pub original: ToFlakeRef,
    #[serde(rename = "originalUrl")]
    pub original_url: String,
    pub path: String,
    pub resolved: ToFlakeRef,
    #[serde(rename = "resolvedUrl")]
    pub resolved_url: String,
    pub url: String,
}

impl TypedCommand for FlakeMetadata {
    type Output = FlakeMetadataOut;
}

/// `nix develop` Command
#[derive(Debug, Default, Clone)]
pub struct Develop {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub installable: InstallableArg,
    pub develop_args: DevelopArgs,
}

impl NixCliCommand for Develop {
    type Own = DevelopArgs;

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLE: Group<Self, InstallableArg> = Some(|d| d.installable.clone());
    const OWN_ARGS: Group<Self, DevelopArgs> = Some(|d| d.develop_args.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["develop"];
}

/// `nix eval` Command
#[derive(Debug, Default, Clone)]
pub struct Eval {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub eval_args: EvalArgs,
}

impl NixCliCommand for Eval {
    type Own = EvalArgs;

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const OWN_ARGS: Group<Self, EvalArgs> = Some(|d| d.eval_args.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["eval"];
}
impl JsonCommand for Eval {}

/// `nix run` Command
#[derive(Debug, Default, Clone)]
pub struct Run {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub installable: InstallableArg,
}

impl NixCliCommand for Run {
    type Own = ();

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLE: Group<Self, InstallableArg> = Some(|d| d.installable.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["run"];
}
impl JsonCommand for Run {}
impl TypedCommand for Run {
    type Output = ();
}

/// `nix shell` Command
#[derive(Debug, Default, Clone)]
pub struct Shell {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub installables: InstallablesArgs,
}

impl NixCliCommand for Shell {
    type Own = ();

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLES: Group<Self, InstallablesArgs> = Some(|d| d.installables.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["shell"];
}
impl JsonCommand for Shell {}
impl TypedCommand for Shell {
    type Output = ();
}

/// `nix bundle` Command
#[derive(Debug, Default, Clone)]
pub struct Bundle {
    pub flake: FlakeArgs,
    pub eval: EvaluationArgs,
    pub source: SourceArgs,
    pub installable: InstallableArg,
    pub bundle_args: BundleArgs,
}

impl NixCliCommand for Bundle {
    type Own = BundleArgs;

    const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
    const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
    const INSTALLABLE: Group<Self, InstallableArg> = Some(|d| d.installable.clone());
    const OWN_ARGS: Group<Self, BundleArgs> = Some(|d| d.bundle_args.clone());
    const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
    const SUBCOMMAND: &'static [&'static str] = &["bundle"];
}
impl JsonCommand for Bundle {}
impl TypedCommand for Bundle {
    type Output = ();
}
