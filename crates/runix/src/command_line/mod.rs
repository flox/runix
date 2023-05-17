//! The [NixCommandLine] backend
//!
//! This module defines the types and traits that drive CLI invocations.
//! Note also the blanket implementation of the [Run] traits below.

use core::fmt;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::process::{ExitStatus, Output, Stdio};

use async_trait::async_trait;
use log::{debug, log};
use serde::Deserialize;
use serde_json::Value;
use thiserror::Error;
use tokio::process::Command;

use crate::arguments::common::NixCommonArgs;
use crate::arguments::config::NixConfigArgs;
use crate::arguments::eval::EvaluationArgs;
use crate::arguments::flake::FlakeArgs;
use crate::arguments::source::SourceArgs;
use crate::arguments::{InstallableArg, InstallablesArgs, NixArgs};
use crate::{NixBackend, Run, RunJson, RunTyped};

pub mod flag;

/// Defaults for all option groups
///
/// The implemetation of [Run] for [NixCommandLine] ensures
/// that only defaults for the groups applicable to the command are issued.
#[derive(Clone, Debug, Default)]
pub struct DefaultArgs {
    pub environment: HashMap<String, String>,
    pub common_args: NixCommonArgs,
    pub config_args: NixConfigArgs,
    pub flake_args: FlakeArgs,
    pub eval_args: EvaluationArgs,
    pub extra_args: Vec<String>,
}

/// Errors occuring during the execution of the Nix Cli
#[derive(Error, Debug)]
pub enum NixCommandLineError {
    #[error("Error running Nix: {0}")]
    Run(std::io::Error),
    /// unsused
    #[deprecated]
    #[error("Nix printed {0} bytes to stderr")]
    Printed(u32),
    /// unused
    #[deprecated]
    #[error("Bad exit: {0:?}")]
    Exit(std::process::Output),
}

/// Nix Implementation based on the Nix Command Line
#[derive(Clone, Debug, Default)]
pub struct NixCommandLine {
    pub nix_bin: Option<String>,
    pub defaults: DefaultArgs,
}

/// An extensioon trait for [std::process::Command]
///
/// Adds a `POSIX` style logging function.
pub trait CommandExt {
    fn log(&self, _level: log::Level) {}
}

impl CommandExt for std::process::Command {
    fn log(&self, level: log::Level) {
        debug!(
            "Invoking {executable}:\nenv = {env:?}\nargs = {args:#?}",
            executable = shell_escape::escape(self.get_program().to_string_lossy()),
            env = self.get_envs().into_iter().collect::<HashMap<_, _>>(),
            args = self
                .get_args()
                .map(|arg| arg.to_string_lossy().to_string())
                .collect::<Vec<_>>(),
        );

        if log::log_enabled!(target: "posix", level) {
            log!(
                target: "posix",

                level,

                "{env_string} {executable} {command_string}",

                env_string = self
                    .get_envs()
                    .map(|(k, v)| {
                        format!(
                            "{}={:?}",
                            k.to_string_lossy(),
                            v.map_or("".into(), OsStr::to_string_lossy)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" "),
                executable = shell_escape::escape(self.get_program().to_string_lossy()),
                command_string = self
                    .get_args()
                    .map(|arg| shell_escape::escape(arg.to_string_lossy()))
                    .collect::<Vec<_>>()
                    .join(" ")
            );
        }
    }
}

/// Run commands in different modes.
///
/// Internal interface usied in the blanket impl of [Run]/[RunJson]
/// Mainly used for influencing the destination of `Stdio`
/// See [Collect] and [Passthru] as examples.
#[async_trait]
trait CommandMode {
    type Output;
    type Error;
    async fn run(command: &mut Command) -> Result<Self::Output, Self::Error>;
}

/// Errors occuring during command exection bin [Collect] Mode
#[derive(Error, Debug)]
pub enum NixCommandLineCollectError {
    #[error(transparent)]
    CommandLine(#[from] NixCommandLineError),
    #[error("Nix failed with: [{0}]")]
    NixError(ExitStatus),
}

/// Implementation of a command execution that collects stdout of a process
/// and logs the stderr of the executed subprocess to the logging framework
/// of the host process.
///
/// Silent, non user facing operation
struct Collect;
#[async_trait]
impl CommandMode for Collect {
    type Error = NixCommandLineCollectError;
    type Output = Output;

    async fn run(command: &mut Command) -> Result<Self::Output, NixCommandLineCollectError> {
        command.as_std().log(log::Level::Debug);

        let command = command
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .stdin(Stdio::inherit());

        let child = command.spawn().map_err(NixCommandLineError::Run)?;

        let output = child
            .wait_with_output()
            .await
            .map_err(NixCommandLineError::Run)?;

        if !output.status.success() {
            return Err(NixCommandLineCollectError::NixError(output.status));
        }

        Ok(output)
    }
}

/// Implementation of a command execution that connects the subprocess' stdio
/// to the parent process stdio.
///
/// User facing operation
struct Passthru;
#[async_trait]
impl CommandMode for Passthru {
    type Error = NixCommandLineError;
    type Output = ExitStatus;

    async fn run(command: &mut Command) -> Result<ExitStatus, Self::Error> {
        command.as_std().log(log::Level::Info);

        let command = command
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .stdin(Stdio::inherit());

        let status = command.status().await.map_err(NixCommandLineError::Run)?;

        Ok(status)
    }
}

impl NixCommandLine {
    /// Small wrapping helper function to make Run implementations simpler
    async fn run_command<M: CommandMode, A, B: NixCliCommand<Own = A>>(
        &self,
        command: &B,
        nix_args: &NixArgs,
        json: bool,
    ) -> Result<M::Output, M::Error> {
        let args = vec![
            // apply default args always applicable
            self.defaults.config_args.to_args(),
            self.defaults.common_args.to_args(),
            nix_args.to_args(),
            B::SUBCOMMAND.iter().map(ToString::to_string).collect(),
            // apply command specific defaults if applicable
            // as defined by the command impl
            B::EVAL_ARGS
                .map(|_| self.defaults.eval_args.to_args())
                .unwrap_or_default(),
            B::FLAKE_ARGS
                .map(|_| self.defaults.flake_args.to_args())
                .unwrap_or_default(),
            if json {
                vec!["--json".to_string()]
            } else {
                vec![]
            },
            command.args(),
            self.defaults.extra_args.clone(),
        ];

        let mut command = Command::new(self.nix_bin.as_deref().unwrap_or("nix"));
        command
            .envs(&self.defaults.environment)
            .args(args.into_iter().flatten());

        if let Some(ref cwd) = nix_args.cwd {
            command.current_dir(cwd);
        }

        M::run(&mut command).await
    }
}

/// Create a list of arguments from `&Self`
///
/// Groups, i.e. Structs that only contain fields which immplement [ToArgs]
/// can derive this trait using [runix_derive::ToArgs].
pub trait ToArgs {
    fn to_args(&self) -> Vec<String>;
}

impl<T: ToArgs> ToArgs for Option<T> {
    fn to_args(&self) -> Vec<String> {
        self.iter().flat_map(|t| t.to_args()).collect()
    }
}

impl<T: ToArgs> ToArgs for Vec<T> {
    fn to_args(&self) -> Vec<String> {
        self.iter().flat_map(|t| t.to_args()).collect()
    }
}

/// A group of options that may or may not applicable for a command
///
/// If [Some], provides a function that extracts a group `U`
/// from a refernece to the command.
///
/// If [None], no option of this group is generated on the command line.
/// Any defaults set on the [NixCommandLine] instance for this group
///  will likewise be ignored.
pub type Group<T, U> = Option<fn(&T) -> U>;

/// Marker trait for abstract commands run on the nix CLI
///
/// A new command can be implemented by implementing this trait,
/// and supplying the applicable option groups.
///
/// Used to automatically implement [Run<NixCommandLine>] for implementers
///
/// See the implementations in [super::command] for examples of practical
/// implementations.
pub trait NixCliCommand: fmt::Debug + Sized {
    type Own: ToArgs;

    const SUBCOMMAND: &'static [&'static str];

    const INSTALLABLES: Group<Self, InstallablesArgs> = None;
    const INSTALLABLE: Group<Self, InstallableArg> = None;
    const FLAKE_ARGS: Group<Self, FlakeArgs> = None;
    const EVAL_ARGS: Group<Self, EvaluationArgs> = None;
    const SOURCE_ARGS: Group<Self, SourceArgs> = None;
    const OWN_ARGS: Group<Self, Self::Own> = None;

    fn args(&self) -> Vec<String> {
        let mut acc = Vec::new();
        acc.append(&mut Self::FLAKE_ARGS.map_or(Vec::new(), |f| f(self).to_args()));
        acc.append(&mut Self::EVAL_ARGS.map_or(Vec::new(), |f| f(self).to_args()));
        acc.append(&mut Self::SOURCE_ARGS.map_or(Vec::new(), |f| f(self).to_args()));
        acc.append(&mut Self::INSTALLABLES.map_or(Vec::new(), |f| f(self).to_args()));
        acc.append(&mut Self::INSTALLABLE.map_or(Vec::new(), |f| f(self).to_args()));
        acc.append(&mut Self::OWN_ARGS.map_or(Vec::new(), |f| f(self).to_args()));
        acc
    }
}

/// Marker Trait for commands that can return JSON
///
/// Used to automatically implement [RunJson] for the implementer
/// by adding `--json` to the nix backend invocation
///
/// Commands that may output json data but doing so other than with `--json`
/// should implment [RunJson] directly instead of this marker.
pub trait JsonCommand {}

/// Marker Trait for commands that can be deserialized into
/// [TypedCommand::Output]
///
/// Used to automatically implement [RunTyped] for implementers
/// that implement [RunJson] by trying to deserialize
/// the json output to its associated type
///
/// Commands that may be deserialized from other data than JSON
/// should implment [RunTyped] directly instead of this marker
/// eg for [Develop]:
///
/// ```
/// # use async_trait::async_trait;
/// # use runix::arguments::NixArgs;
/// # use runix::command::Develop;
/// # use runix::{NixBackend, Run, RunTyped};
///
/// # struct NixCommandLine;
/// # impl NixBackend for NixCommandLine {}
///
/// # #[async_trait]
/// # impl Run<NixCommandLine> for Develop {
/// #     type Error = std::io::Error;
///
/// #     async fn run(
/// #         &self,
/// #         backend: &NixCommandLine,
/// #         nix_args: &NixArgs,
/// #     ) -> Result<(), Self::Error> {
/// #         todo!()
/// #     }
/// # }
///
/// #[async_trait]
/// impl RunTyped<NixCommandLine> for Develop {
///     type Output = ();
///     type TypedError = std::io::Error;
///
///     async fn run_typed(
///         &self,
///         backend: &NixCommandLine,
///         nix_args: &NixArgs,
///     ) -> Result<Self::Output, Self::Error> {
///         todo!()
///     }
/// }
/// ```
pub trait TypedCommand {
    type Output;
}

impl ToArgs for () {
    fn to_args(&self) -> Vec<String> {
        Default::default()
    }
}

impl NixBackend for NixCommandLine {}

#[derive(Error, Debug)]
pub enum NixCommandLineRunError {
    #[error("An error occured in CommandLine backend: {0}")]
    Backend(#[from] NixCommandLineError),
    #[error("Nix call unsuccessful: [{0}]")]
    Exit(ExitStatus),
}

#[async_trait]
impl<C> Run<NixCommandLine> for C
where
    C: NixCliCommand + Send + Sync,
{
    type Error = NixCommandLineRunError;

    async fn run(
        &self,
        backend: &NixCommandLine,
        nix_args: &NixArgs,
    ) -> Result<(), NixCommandLineRunError> {
        let exit_status = backend
            .run_command::<Passthru, _, _>(self, nix_args, false)
            .await?;

        if !exit_status.success() {
            Err(NixCommandLineRunError::Exit(exit_status))?
        }

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum NixCommandLineRunJsonError {
    #[error("Error decoding json: {0}")]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Run(NixCommandLineCollectError),
}

#[async_trait]
impl<C> RunJson<NixCommandLine> for C
where
    C: NixCliCommand + JsonCommand + Send + Sync,
{
    type JsonError = NixCommandLineRunJsonError;

    async fn run_json(
        &self,
        backend: &NixCommandLine,
        nix_args: &NixArgs,
    ) -> Result<Value, Self::JsonError> {
        let output = backend
            .run_command::<Collect, _, _>(self, nix_args, true)
            .await
            .map_err(NixCommandLineRunJsonError::Run)?;

        let out_str = String::from_utf8_lossy(&output.stdout);
        debug!("JSON command output: {:?}", out_str);

        Ok(serde_json::from_str(&out_str)?)
    }
}

#[async_trait]
impl<C> RunTyped<NixCommandLine> for C
where
    C: RunJson<NixCommandLine> + TypedCommand + Send + Sync,
    <C as TypedCommand>::Output: for<'de> Deserialize<'de>,
{
    type Output = C::Output;
    type TypedError = <Self as RunJson<NixCommandLine>>::JsonError;

    async fn run_typed(
        &self,
        backend: &NixCommandLine,
        nix_args: &NixArgs,
    ) -> Result<Self::Output, Self::TypedError> {
        match self.run_json(backend, nix_args).await {
            Ok(v) => Ok(serde_json::from_value(v).unwrap()),
            Err(e) => Err(e),
        }
    }
}
