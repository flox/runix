//! Legacy arguments, see [LegacyArgs]

use derive_more::{Deref, From};
use runix_derive::ToArgs;

use crate::command_line::ToArgs;
use crate::default::flag::{Flag, FlagType};

/// Legacy arguments
/// Corresponding to the arguments defined in
/// [libmain/shared.cc](https://github.com/NixOS/nix/blob/2d1d81114d72ace89ce08cd3bc93f4eb27a2975d/src/libmain/shared.cc#L177-L245)
#[derive(Clone, Default, Debug, ToArgs)]
pub struct LegacyArgs {
    pub store: Store,
}

#[derive(Clone, From, Debug, Deref, Default)]
pub struct Store(String);
impl Flag for Store {
    const FLAG: &'static str = "--store";
    const FLAG_TYPE: FlagType<Self> = FlagType::arg();
}
