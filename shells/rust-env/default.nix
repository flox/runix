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
  # commitizen,
}:
mkShell {
  inputsFrom = [];
  RUST_SRC_PATH = rust.packages.stable.rustPlatform.rustLibSrc;
  RUSTFMT = "${self.checks.pre-commit-check.passthru.rustfmt}/bin/rustfmt";
  packages =
    [
      # temporary until https://github.com/commitizen-tools/commitizen/pull/644 is merged
      self.checks.pre-commit-check.passthru.commitizen
      self.checks.pre-commit-check.passthru.rustfmt

      cargo
      rustc
      clippy
      rust-analyzer
      rust.packages.stable.rustPlatform.rustLibSrc
    ]
    ++ lib.optional hostPlatform.isDarwin [
      darwin.apple_sdk.frameworks.Security
      libiconv
    ];

  shellHook = ''
    ${self.checks.pre-commit-check.shellHook}
  '';
}
