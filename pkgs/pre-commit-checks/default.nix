{
  inputs,
  commitizen-with-bump,
  rustfmt-nightly,
}:
inputs.shellHooks.lib.run {
  src = ../..;
  hooks = {
    alejandra.enable = true;
    rustfmt.enable = true;
    clippy.enable = true;
    commitizen.enable = true;
  };
  settings.clippy.denyWarnings = true;
  tools = {
    rustfmt = rustfmt-nightly;
    commitizen = commitizen-with-bump;
  };
}
