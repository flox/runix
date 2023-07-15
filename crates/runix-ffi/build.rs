fn main() {
    cxx_build::bridge("src/lib.rs")
        .file("src/nix.cpp")
        .flag_if_supported("-std=c++20")
        .flag_if_supported("-O2")
        .cpp(true)
        .includes(pkg_config::probe_library("nix-main").unwrap().include_paths)
        .compile("runix-ffi");
    println!("cargo:rustc-link-lib=nixfetchers");

    println!("cargo:rerun-if-changed=include/nix.h");
    println!("cargo:rerun-if-changed=src/nix.cpp");
    println!("cargo:rerun-if-changed=src/lib.rs");

    pkg_config::probe_library("nix-main").unwrap();
    pkg_config::probe_library("nix-cmd").unwrap();
    pkg_config::probe_library("nix-expr").unwrap();
    pkg_config::probe_library("nix-store").unwrap();
}
