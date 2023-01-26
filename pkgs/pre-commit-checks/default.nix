{
  inputs,
  commitizen,
  rustfmt,
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
  tools = {inherit commitizen rustfmt;};
}
