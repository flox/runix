//! runix is a library that allows you to **run** **nix** using a typed
//! interface.
//!
//! runix converts command structures into an invocation of a [NixBackend]
//! implementation.
//! The backend currently in development is the [command_line::NixCommandLine]
//! backend, which uses [tokio::process::Command] to `exec` the nix CLI.
//!
//! While this is the reference implmentation, other backends such as an
//! FFI based implementation or Mocking shims for testing are possible.
//!
//! > **Warning**
//! > runix is still in active development!
//! >
//! > It's API is not yet deeply set in stone, more fields will be added as we
//! > expand the coverage of the Nix CLI and traits _may_ change if necessary.
//! >
//! > We greatly appreciate feedback and contributions.
//!
//! # Examples
//!
//! The easiest way to get familiar with the interface is by way of an example.
//!
//! Again, mind that you'll need nix on your `PATH` to use runix.
//!
//! ```no_run
//! # use runix::arguments::source::SourceArgs;
//! # use runix::arguments::NixArgs;
//! # use runix::command::Eval;
//! # use runix::command_line::NixCommandLine;
//! # use runix::Run;
//! # #[tokio::main]
//! # async fn main() {
//! // (1) initialize a backend
//! let cli = NixCommandLine::default();
//!
//! // (2) define the command
//! Eval {
//!     source: SourceArgs {
//!         expr: Some(r#""Hello Rust""#.into()),
//!     },
//!     ..Default::default()
//! }
//! // (3) run the command
//! .run(&cli, &NixArgs::default())
//! .await
//! #    .unwrap();
//! # }
//! ```
//!
//! This is the runix equivalent to:
//!
//! ```shell
//! $ nix eval --expr '"Hello Rust"'
//! ```
//!
//! While certainly more wordy, than its shell counterpart, its comparable to
//! the same invocation, written manually:
//!
//! ```
//! # async fn with_tokio() {
//! tokio::process::Command::new("nix")
//!     .args([
//!         "eval".to_string(),
//!         "--expr".into(),
//!         r#""Hello Rust""#.into(),
//!     ])
//!     .status()
//!     .await
//! #    .unwrap();
//! # }
//! ```
//!
//! The main benefit of runix however is that it abstracts away the plain list
//! of arguments and guides you to correct invocations of the cli.
//!
//! # Design goals: Correct, Flexible, Extensible
//!
//! runix' core-interface are the [Run] traits;
//! [Run], [RunJson] and [RunTyped].
//! These traits are implmented for a command and are parametrized by a
//! [NixBackend].
//!
//! runix ships with a backend implementation for the Nix CLI and associated
//! [Run] implementations.
//! The [command_line::NixCliCommand] trait describes an abstract CLI
//! invocation.
//! An implementation of the trait defines how a command type is converted to
//! list of arguments.
//! Many Nix commands share different subsets of arguments - implemented as
//! mixins in Nix' C++ code base.
//! In runix these option groups are represented by individual types.
//!
//! - common arguments valid for all nix commands
//!   ([arguments::common::NixCommonArgs])
//! - Nix onfiguration options and nix.conf values
//!   ([arguments::config::NixConfigArgs])
//! - arguments for commands that evaluate expressions
//!   ([arguments::source::SourceArgs])
//! - flake related options ([arguments::flake::FlakeArgs])
//! - installables:
//!   ([arguments::InstallableArg] & [arguments::InstallablesArgs])
//!
//! An implementation of a [command_line::NixCliCommand] then defines
//! which of these groups are applicable,
//! and how they are extracted from an instance of the type.
//! Additionally, some commands have their own specific options that do not
//! fall into one of the larger groups.
//!
//! Splitting groups and in the same way as nix does internally helps runix
//! to generate _correct_ invocations.
//!
//! An exemplary [command_line::NixCliCommand] looks as follows
//!
//! ```
//! # use runix::arguments::InstallablesArgs;
//! # use runix::arguments::eval::EvaluationArgs;
//! # use runix::arguments::flake::FlakeArgs;
//! # use runix::arguments::source::SourceArgs;
//! # use runix::command_line::NixCliCommand;
//! # use runix::command_line::Group;

//!
//! // The command interface
//! #[derive(Debug, Default, Clone)]
//! pub struct Shell {
//!     pub flake: FlakeArgs,
//!     pub eval: EvaluationArgs,
//!     pub source: SourceArgs,
//!     pub installables: InstallablesArgs,
//! }
//!
//! impl NixCliCommand for Shell {
//!     type Own = (); // no specific arguments for Shell
//!
//!     // the nix subcommand (`nix shell`)
//!     const SUBCOMMAND: &'static [&'static str] = &["shell"];
//!
//!     // shell supports three groups of options and multiple installables
//!     const EVAL_ARGS: Group<Self, EvaluationArgs> = Some(|d| d.eval.clone());
//!     const FLAKE_ARGS: Group<Self, FlakeArgs> = Some(|d| d.flake.clone());
//!     const INSTALLABLES: Group<Self, InstallablesArgs> = Some(|d| d.installables.clone());
//!     const SOURCE_ARGS: Group<Self, SourceArgs> = Some(|d| d.source.clone());
//! }
//! ```
//! 
//! Note that not all groups need to be specified.
//! The trait implements defaults if they are not applicable.
//!
//! All option groups implement the [command_line::ToArgs] trait,
//! to convert the groups to a list of CLI arguments.
//! The fields on an option group typically implement
//! [command_line::flag::Flag].
//! This trait allows declarative definitions of individual flags,
//! and their value formating.
//! A [command_line::flag::Flag] is comprised of an option
//! ([command_line::flag::Flag::FLAG]) and a conversion of the type's content
//! to a list of arguments (driven by [command_line::flag::FlagType]).
//! [command_line::flag::FlagType] implementes the conversion different kinds
//! of arguments, including flags, lists, paths, numbers or manual layouts.
//! ```no_run
//! # use runix::command_line::flag::{Flag, FlagType};
//! # use derive_more::{Deref, From};
//!
//! /// Flag for accept-flake-config
//! #[derive(Clone, From, Debug, Deref, Default)]
//! pub struct ConnectTimeout(u32);
//! impl Flag for ConnectTimeout {
//!     const FLAG: &'static str = "--connect-timeout";
//!     const FLAG_TYPE: FlagType<Self> = FlagType::number_arg();
//! }
//! ```
//! 
//! All implementors of [command_line::flag::Flag] automatically
//! implement [command_line::ToArgs].
//!
//! If all fields of a group implement [command_line::ToArgs],
//! [command_line::ToArgs] can be `derive`d for the entire Group using
//! [runix_derive::ToArgs].
//!
//! runix ships with an initial set of commands,
//! which is bound to grow over time.
//! You can implement commands that are not yet supported in runix
//! in your own project,
//! by defining a type and implementing [command_line::NixCliCommand].
//!
//! You are welcome to contribute those implemntations back to runix!
//!
//! # Future Roadmap
//!
//! We plan to expand the command line backend with more commands and
//! a comprehensive set of flags.
//! Depending on the state of abstractions in Nix,
//! we plan to approach native bindings to Nix commands and concepts.

use std::error::Error;

/// Rust abstraction over the nix command line
/// Candidate for a standalone library to build arbitrary Nix commands in a safe manner
use arguments::NixArgs;
use async_trait::async_trait;

pub mod arguments;
pub mod command;
pub mod command_line;
// pub mod flake_ref;
pub mod flake_ref2;
pub mod installable;
pub mod registry;

pub use command_line as default;
use serde_json::Value;

/// Marker trait for Nix Backends
///
/// [Run], [RunJson] and [RunTyped] require a [NixBackend].
/// This may be used in the future for cross backend compatibility.
pub trait NixBackend {}

/// Core trait of runix
///
/// Implemented for commands that may take a reference of a [NixBackend]
/// to run a nix command.
///
/// # Example
///
/// Following example implements [Run] for a `Command`
/// on the `MyBackend` backend.
///
/// ```no_run
/// # use runix::{NixBackend, Run};
/// # use runix::arguments::NixArgs;
/// # use runix::command_line::NixCliCommand;
/// # use runix::command::Build;
/// # use std::io;
///
/// struct MyBackend;
/// struct Command;
/// impl NixBackend for MyBackend {}
///
/// #[async_trait::async_trait]
/// impl Run<MyBackend> for Command {
///     type Error = io::Error;
///
///     async fn run(&self, _backend: &MyBackend, _nix_args: &NixArgs) -> Result<(), io::Error> {
///         panic!("42")
///         // backend.run_in_nix(args)
///     }
/// }
///
/// #[tokio::main]
/// async fn main() {
///     Command
///         .run(&MyBackend, &NixArgs {
///             ..Default::default()
///         })
///         .await
///         .unwrap()
/// }
/// ```
#[async_trait]
pub trait Run<B: NixBackend> {
    type Error: 'static + Error + Send + Sync;
    async fn run(&self, backend: &B, nix_args: &NixArgs) -> Result<(), Self::Error>;
}

/// Specialized version of [Run] that guarantees JSON output
#[async_trait]
pub trait RunJson<B: NixBackend>: Run<B> {
    type JsonError: 'static + Error + Send + Sync;
    async fn run_json(&self, backend: &B, nix_args: &NixArgs) -> Result<Value, Self::JsonError>;
}

/// Specialized version of [Run] that guarantees an associated type as output
#[async_trait]
pub trait RunTyped<B: NixBackend>: Run<B> {
    type Output;
    type TypedError: 'static + Error + Send + Sync;
    async fn run_typed(
        &self,
        backend: &B,
        nix_args: &NixArgs,
    ) -> Result<Self::Output, Self::TypedError>;
}
