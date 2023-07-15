use thiserror::Error;

#[cxx::bridge(namespace = "runix")]
mod ffi {
    unsafe extern "C++" {
        include!("runix-ffi/include/nix.h");
    }
}

#[derive(Debug, Error)]
pub enum FfiError {
    #[error("{0}")]
    NixException(String),
    #[error("Failed parsing ffi result: {0}")]
    Parse(serde_json::Error),
}
