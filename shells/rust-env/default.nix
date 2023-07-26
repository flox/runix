{
  mkShell,
  self,
  runix,
  rustPlatform,
  cargo,
  rustc,
  rust-analyzer,
  rustfmt-nightly,
  pre-commit-checks,
}:
mkShell {
  inputsFrom = [runix];
  RUSTFMT = rustfmt-nightly.outPath + "/bin/rustfmt";
  packages = [cargo rustc rust-analyzer rustPlatform.rustLibSrc];
  inherit (runix) RUST_SRC_PATH PARSER_UTIL;
  inherit (pre-commit-checks) shellHook;
}
