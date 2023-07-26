{
  mkShell,
  commitizen-with-bump,
  rustfmt-nightly,
}:
mkShell {
  inputsFrom = [];
  packages = [commitizen-with-bump rustfmt-nightly];
}
