{
  mkShell,
  self,
  lib,
  clippy,
  rust-analyzer,
  darwin,
  hostPlatform,
  cargo,
  rustc,
  rust,
  libiconv,
  commitizen,
  rustfmt,
  pre-commit-checks,
}:
mkShell {
  inputsFrom = [];
  RUST_SRC_PATH = rust.packages.stable.rustPlatform.rustLibSrc;
  RUSTFMT = "${rustfmt}/bin/rustfmt";
  packages =
    [
      rustfmt
      commitizen
      cargo
      rustc
      clippy
      rust-analyzer
    ]
    ++ lib.optional hostPlatform.isDarwin [
      darwin.apple_sdk.frameworks.Security
      libiconv
    ];

  shellHook = ''
    ${pre-commit-checks.shellHook}
  '';
}
