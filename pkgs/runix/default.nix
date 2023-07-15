{
  pkgs,
  nix,
  pkg-config,
  boost,
  nlohmann_json,
  rustfmt,
}:
pkgs.mkShell {
  name = "runix-devenv-shim";
  nativeBuildInputs = with pkgs; [rustc cargo pkg-config];
  buildInputs = with pkgs; [
    nix
    nlohmann_json
    libsodium
    boost
    rustfmt
    rust-analyzer

    openssl
  ];

  # provide a dummy configuration for testing
  CONFIG_FILE = pkgs.writeText "config.toml" "";

  RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
}
# runCommandCC "runix-devenv-shim" {nativeBuildInputs = [pkg-config]; buildInputs = [nix boost nlohmann_json];} ''
#   cat <<EOF
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                           Dev env only package
#     runix is a library distributed through crates.io.
#     This package is required to provide a flox development environment.
#     If you tried to build this package, instead run:
#         $ flox develop runix
#   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#   EOF
#   exit 2
# ''

