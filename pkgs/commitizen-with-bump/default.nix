{commitizen}:
commitizen.overridePythonAttrs (old: {
  doCheck = false;
  src = builtins.fetchGit {
    url = "https://github.com/skoef/commitizen/";
    ref = "add-hooks-for-bump-command";
    rev = "26b38beb8d507e4a4ee3c062639a96230c33dd92";
  };
  meta = (old.meta or {}) // {mainProgram = "cz";};
})
