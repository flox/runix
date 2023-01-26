{runCommandNoCC}:
runCommandNoCC "runix-devenv-shim" {} ''
  cat <<EOF
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                          Dev env only package

    runix is a library distributed through crates.io.
    This package is required to provide a flox development environment.
    If you tried to build this package, instead run:

        $ flox develop runix

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  EOF
  exit 2
''
