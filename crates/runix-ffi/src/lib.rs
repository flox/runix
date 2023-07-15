use serde_json::Value;
use thiserror::Error;

#[cxx::bridge(namespace = "runix")]
mod ffi {
    unsafe extern "C++" {
        include!("runix-ffi/include/nix.h");
        fn parse_flakeref(url: &str) -> Result<String>;
    }
}

#[derive(Debug, Error)]
pub enum FfiError {
    #[error("{0}")]
    NixException(String),
    #[error("Failed parsing ffi result: {0}")]
    Parse(serde_json::Error),
}

pub fn parse_flakeref(url: impl AsRef<str>) -> Result<Value, FfiError> {
    ffi::parse_flakeref(url.as_ref())
        .map_err(|ex| FfiError::NixException(ex.to_string()))
        .and_then(|ref resolved| serde_json::from_str(resolved).map_err(FfiError::Parse))
}

#[test]
fn test_parse_flakeref() {
    match parse_flakeref("flake:bla") {
        Ok(json) => println!("✅ {json}"),
        Err(e) => println!("🤷🏼 {e:?}"),
    };
}
