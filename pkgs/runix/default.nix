{
  self,
  lib,
  rustPlatform,
  hostPlatform,
  libiconv,
  darwin,
  cargo,
  rustc,
  clippy,
  inputs,
  parser-util ? inputs.parser-util.packages.parser-util,
  rustfmt-nightly,
  commitizen-with-bump,
}:
rustPlatform.buildRustPackage {
  pname = "runix";
  version = "0.1.1-${lib.flox-floxpkgs.getRev self}";
  src = self;

  cargoLock = {
    lockFile = self + "/Cargo.lock";
    # The hash of each dependency that uses a git source must be specified.
    # The hash can be found by setting it to lib.fakeSha256
    # as shown below and running flox build.
    # The build will fail but output the expected sha, which can then be added
    # here
    outputHashes = {
      #   "dependency-0.0.0" = lib.fakeSha256;
    };
  };

  # Non-Rust runtime dependencies (most likely libraries) of your project can
  # be added in buildInputs.
  # Make sure to import any additional dependencies above
  buildInputs = lib.optional hostPlatform.isDarwin [
    libiconv
    darwin.apple_sdk.frameworks.Security
  ];

  propagatedBuildInputs = [parser-util];
  nativeBuildInputs = [commitizen-with-bump clippy rustfmt-nightly];

  RUST_SRC_PATH = rustPlatform.rustLibSrc.outPath;
  PARSER_UTIL = parser-util.outPath + "/bin/parser-util";
}
