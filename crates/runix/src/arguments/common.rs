//! Arguments common for all mix commands

use derive_more::{Deref, From};
use runix_derive::ToArgs;

use crate::command_line::flag::{Flag, FlagType};
use crate::command_line::ToArgs;

/// These arguments do not depend on the nix subcommand issued
/// and refer to the options defined in
/// - [libmain/common-args.cc](https://github.com/NixOS/nix/blob/a6239eb5700ebb85b47bb5f12366404448361f8d/src/libmain/common-args.cc#L7-L81)
/// - [libmain/shared.cc](https://github.com/NixOS/nix/blob/2d1d81114d72ace89ce08cd3bc93f4eb27a2975d/src/libmain/shared.cc#L177-L245)
/// - [nix/main.cc](https://github.com/NixOS/nix/blob/b7e8a3bf4cbb2448db860f65ea13ef2c64b6883b/src/nix/main.cc#L66-L110)
#[derive(Clone, Default, Debug, ToArgs)]
pub struct NixCommonArgs {
    pub store: Option<Store>,
}

#[derive(Clone, From, Debug, Deref, Default)]
pub struct Store(String);
impl Flag for Store {
    const FLAG: &'static str = "--store";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}
