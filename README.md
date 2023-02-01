# runix

A typesafe interface to the [nix](https://github.com/nixos/nix)

*by flox*

------

## Installation

Install with `cargo add` (Rust >= 1.64)

```shell
cargo add runix
```

Alternatively, manually add

```toml
runix = "{{current_version}}" # check the latest version before adding
```

to your `Cargo.toml`.


*runix* requires an existing [nix](https://github.com/nixos/nix) installation.

runix is a library that allows you to **run** **nix** using a typed
interface.

runix converts command structures into an invocation of a [NixBackend]
implementation.
The backend currently in development is the [command_line::NixCommandLine]
backend, which uses [tokio::process::Command] to `exec` the nix CLI.

While this is the reference implmentation, other backends such as an
FFI based implementation or Mocking shims for testing are possible.

> **Warning**
> runix is still in active development!
>
> It's API is not yet deeply set in stone, more fields will be added as we
> expand the coverage of the Nix CLI and traits _may_ change if necessary.
>
> We greatly appreciate feedback and contributions.

## Examples

The easiest way to get familiar with the interface is by way of an example.

Again, mind that you'll need nix on your `PATH` to use runix.

```rust
// (1) initialize a backend
let cli = NixCommandLine::default();

// (2) define the command
Eval {
    source: SourceArgs {
        expr: Some(r#""Hello Rust""#.into()),
    },
    ..Default::default()
}
// (3) run the command
.run(&cli, &NixArgs::default())
.await
```

This is the runix equivalent to:

```shell
$ nix eval --expr '"Hello Rust"'
```

While certainly more wordy, than its shell counterpart, its comparable to
the same invocation, written manually:

```rust
tokio::process::Command::new("nix")
    .args([
        "eval".to_string(),
        "--expr".into(),
        r#""Hello Rust""#.into(),
    ])
    .status()
    .await
```

The main benefit of runix however is that it abstracts away the plain list
of arguments and guides you to correct invocations of the cli.

**Interested?**: checkout the rustdoc for more detailed explanations.


## Future Roadmap

We plan to expand the command line backend with more commands and
a comprehensive set of flags.
Depending on the state of abstractions in Nix,
we plan to approach native bindings to Nix commands and concepts.

License: LGPL-2.1
