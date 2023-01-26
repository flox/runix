{
  context,
  system,
  ...
}: let
  context' = context.context' system;
  libiconv = {libiconv}: libiconv;
  Security = {darwin}: darwin.apple_sdk.frameworks.Security;
  darwinOnly = {inherit libiconv Security;};
in {
  packages.nixpkgs-flox = {
    cargo = {};
    rustc = {};
    clippy = {};
  };

  inline.packages."x86_64-darwin" = darwinOnly;
  inline.packages."aarch64-darwin" = darwinOnly;

  packages.self.rustfmt = {};
  packages.self.commitizen = {};

  shell.hook = ''
    ${context'.self.packages.pre-commit-checks.shellHook}
  '';

  environmentVariables = {
    RUST_SRC_PATH = "${context'.nixpkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
  };
}
