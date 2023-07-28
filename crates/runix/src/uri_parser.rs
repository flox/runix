use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::str::FromStr;
use std::string::FromUtf8Error;

use once_cell::sync::OnceCell;
use serde::de::Error;
use serde::{Deserialize, Deserializer};
use serde_json::Value;

use crate::flake_ref::lock::{InvalidRev, LastModified, NarHash, Rev, RevCount};
use crate::flake_ref::protocol::WrappedUrlParseError;
use crate::flake_ref::{ParseTimeError, Timestamp, TimestampDeserialize};

pub(crate) static PARSER_UTIL_BIN_PATH: OnceCell<PathBuf> = OnceCell::new();

/// The various errors that can be encountered parsing the JSON output of `parser-util`.
///
/// Some of these are legitimate errors that can occur deserializing the JSON to structured
/// types. Others are error cases that we inflict upon ourselves trying to strongly type
/// the parsed data we get back from Nix/`parser-util`.
#[derive(Debug, thiserror::Error)]
pub enum UriParseError {
    // These are errors finding or setting the path to the `parser-util` binary
    #[error("could not set the path to the `parser-util` binary")]
    FailedSettingBinPath,
    #[error("could not get the path to the 'parser-util' binary, none provided and PARSER_UTIL_BIN not set")]
    BinPathNotSet,
    // Errors trying to call the parser and read its output
    #[error("calling the parser failed")]
    ParserCall(#[from] std::io::Error),
    #[error("parser did not exit successfully: E{0}: '{1}'")]
    ParserError(ExitStatus, String),
    #[error("the parser did not return valid UTF-8")]
    Utf8(#[from] FromUtf8Error),
    // These are errors deserializing the JSON from `parser-util`
    #[error("protocol '{0}' is invalid for this flake type")]
    InvalidProtocol(String),
    #[error("flake type required a protocol but none was found")]
    ProtocolNotFound,
    #[error("'nix' executable did not return a 'type' attribute for this flake reference")]
    TypeNotFound,
    #[error("expected 'type' to be a string, found {0} instead")]
    TypeWasWrongType(Value),
    #[error("unrecognized flake type '{0}'")]
    UnrecognizedFlakeType(String),
    #[error("failed to deserialize JSON")]
    Deserialize(#[from] serde_json::Error),
    #[error("parsed URI was missing attribute '{0}'")]
    MissingAttribute(String),
    // Everything after this point is self-inflicted trying to strongly type what Nix gave us
    #[error("bad timestamp")]
    BadTimestamp(#[from] ParseTimeError),
    #[error("bad revision")]
    BadRevision(#[from] InvalidRev),
    #[error("unsupported protocol '{1}' for flake type '{0}'")]
    UnsupportedProtocol(String, String),
    #[error("unsupported service '{0}'")]
    UnsupportedService(String),
    #[error("failed to parse URL")]
    URLParseError(#[from] WrappedUrlParseError),
    #[error("attribute '{0}' had unexpected type, expected '{1}' and found '{2}'")]
    AttributeType(String, String, Value),
    #[error("{0}")]
    Other(String),
}

/// A flake reference URI that has been resolved by Nix, but has not been
/// checked that it exists i.e. locked.
#[derive(Debug, Clone)]
pub struct ResolvedFlakeRef {
    pub input: Input,
    pub original_ref: ParsedFlakeReference,
    pub resolved_ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURI> for ResolvedFlakeRef {
    type Error = UriParseError;

    fn try_from(parsed: GenericParsedURI) -> Result<Self, Self::Error> {
        let input = parsed
            .input
            .ok_or_else(|| UriParseError::MissingAttribute("input".to_string()))?;
        let original_ref = parsed
            .original_ref
            .ok_or_else(|| UriParseError::MissingAttribute("originalRef".to_string()))
            .and_then(ParsedFlakeReference::try_from)?;
        let resolved_ref = parsed
            .resolved_ref
            .ok_or_else(|| UriParseError::MissingAttribute("resolvedRef".to_string()))
            .and_then(ParsedFlakeReference::try_from)?;

        Ok(ResolvedFlakeRef {
            input,
            original_ref,
            resolved_ref,
        })
    }
}

/// A flake reference URI that has been resolved and checked by Nix that it exists
/// i.e. locked.
#[derive(Debug, Clone)]
pub struct LockedFlakeRef {
    pub input: Input,
    pub original_ref: ParsedFlakeReference,
    pub resolved_ref: ParsedFlakeReference,
    pub locked_ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURI> for LockedFlakeRef {
    type Error = UriParseError;

    fn try_from(mut parsed: GenericParsedURI) -> Result<Self, Self::Error> {
        let locked_ref = parsed
            .locked_ref
            .take()
            .ok_or_else(|| UriParseError::MissingAttribute("lockedRef".to_string()))
            .and_then(ParsedFlakeReference::try_from)?;

        let ResolvedFlakeRef {
            input,
            original_ref,
            resolved_ref,
        } = ResolvedFlakeRef::try_from(parsed)?;

        Ok(LockedFlakeRef {
            input,
            original_ref,
            resolved_ref,
            locked_ref,
        })
    }
}

/// A flake reference URI pointing to an installable that has been resolved by Nix
#[derive(Debug, Clone)]
pub struct InstallableFlakeRef {
    pub input: Input,
    pub attr_path: Vec<String>,
    pub outputs: InstallableOutputs,
    pub r#ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURI> for InstallableFlakeRef {
    type Error = UriParseError;

    fn try_from(parsed: GenericParsedURI) -> Result<Self, Self::Error> {
        let input = parsed
            .input
            .ok_or_else(|| UriParseError::MissingAttribute("input".to_string()))?;

        let attr_path = parsed
            .attr_path
            .ok_or_else(|| UriParseError::MissingAttribute("attrPath".to_string()))?;

        let outputs = parsed
            .outputs
            .ok_or_else(|| UriParseError::MissingAttribute("outputs".to_string()))?;
        let r#ref = parsed
            .r#ref
            .ok_or_else(|| UriParseError::MissingAttribute("ref".to_string()))
            .and_then(ParsedFlakeReference::try_from)?;

        Ok(InstallableFlakeRef {
            input,
            attr_path,
            outputs,
            r#ref,
        })
    }
}

/// A completely generic parsed URI i.e. a type that can represent a call to `parser_util`
/// with any of the flags
#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
#[allow(dead_code)]
struct GenericParsedURI {
    #[serde(deserialize_with = "deserialize_input")]
    input: Option<Input>,
    original_ref: Option<DeserializedFlakeRef>,
    resolved_ref: Option<DeserializedFlakeRef>,
    locked_ref: Option<DeserializedFlakeRef>,
    r#ref: Option<DeserializedFlakeRef>,
    attr_path: Option<Vec<String>>,
    outputs: Option<InstallableOutputs>,
    authority: Option<Option<String>>,
    base: Option<String>,
    fragment: Option<String>,
    path: Option<String>,
    query: Option<HashMap<String, Option<String>>>,
    scheme: Option<Scheme>,
}

/// A type representing a Nix-parsed URL scheme
#[derive(Debug, Clone, Deserialize)]
pub struct Scheme {
    pub application: Option<String>,
    pub full: String,
    pub transport: String,
}

/// The type and contents of the input the URI parser received.
#[derive(Debug, Deserialize, Clone)]
pub enum Input {
    String(String),
    Attrs(Attrs),
}

/// The "attrs" field in a (resolved|locked) reference can essentially contain
/// arbitrary field names given that these change from Nix version to Nix version.
type Attrs = HashMap<String, Value>;

fn deserialize_input<'de, D>(deserializer: D) -> Result<Option<Input>, D::Error>
where
    D: Deserializer<'de>,
{
    let Some(json_value): Option<Value> = Deserialize::deserialize(deserializer)? else {
        return Ok(None);
    };
    use Value::*;
    match json_value {
        String(string) => Ok(Some(Input::String(string))),
        Object(map) => {
            let attrs = map
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect::<HashMap<_, _>>();
            Ok(Some(Input::Attrs(attrs)))
        },
        _ => Err(D::Error::custom(format!(
            "encountered unknown input type: {:?}",
            json_value
        ))),
    }
}

/// The possible flake output types
#[derive(Debug, Deserialize, Clone)]
pub enum InstallableOutputs {
    All,
    Default,
    Selected(Vec<String>),
}

/// A flake reference that has been parsed by Nix
#[derive(Debug, Deserialize, Clone)]
pub struct DeserializedFlakeRef {
    pub attrs: Attrs,
    pub string: String,
}

/// A strongly-typed flake reference that has already gone through deserialization and validation
#[derive(Debug, Clone)]
pub struct ParsedFlakeReference {
    pub attrs: Attrs,
    pub string: String,
    pub flake_type: FlakeType,
}

impl TryFrom<DeserializedFlakeRef> for ParsedFlakeReference {
    type Error = UriParseError;

    fn try_from(flake: DeserializedFlakeRef) -> Result<Self, Self::Error> {
        use FlakeType::*;
        let Some(raw_flake_type) = flake.attrs.get("type") else {
            return Err(UriParseError::TypeNotFound);
        };
        let Value::String(raw_flake_type) = raw_flake_type else {
            return Err(UriParseError::TypeWasWrongType(raw_flake_type.clone()));
        };
        let protocol = url_scheme(flake.attrs.get("url"));
        match raw_flake_type.as_ref() {
            "git" | "mercurial" | "tarball" | "file" => {
                if protocol.is_none() {
                    return Err(UriParseError::ProtocolNotFound);
                }
            },
            _ => {},
        };
        // Unwrapping `protocol` is now safe because we've verified that it's `Some` for any flake type that needs it
        let flake_type = match raw_flake_type.as_ref() {
            "path" => Path,
            "github" => Github,
            "gitlab" => Gitlab,
            "sourcehut" => Sourcehut,
            "indirect" => Indirect,
            "git" => {
                let p_type = GitProtocolType::from_str(protocol.unwrap().as_ref())?;
                Git(p_type)
            },
            "mercurial" => {
                let p_type = MercurialProtocolType::from_str(protocol.unwrap().as_ref())?;
                Mercurial(p_type)
            },
            "tarball" => {
                let p_type = TarballProtocolType::from_str(protocol.unwrap().as_ref())?;
                Tarball(p_type)
            },
            "file" => {
                let p_type = FileProtocolType::from_str(protocol.unwrap().as_ref())?;
                File(p_type)
            },
            _ => return Err(UriParseError::UnrecognizedFlakeType(raw_flake_type.clone())),
        };
        Ok(ParsedFlakeReference {
            attrs: flake.attrs,
            string: flake.string,
            flake_type,
        })
    }
}

/// Extracts the scheme out of a flake reference URL
fn url_scheme(url: Option<&Value>) -> Option<String> {
    let Some(Value::String(url)) = url else {
        return None;
    };
    url.split(":/").next().map(|s| s.to_string())
}

/// The various types of flake references
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FlakeType {
    Path,
    Git(GitProtocolType),
    Mercurial(MercurialProtocolType),
    Tarball(TarballProtocolType),
    File(FileProtocolType),
    Github,
    Gitlab,
    Sourcehut,
    Indirect,
}

impl From<FlakeType> for String {
    fn from(ft: FlakeType) -> Self {
        match ft {
            FlakeType::Path => "path",
            FlakeType::Git(_) => "git",
            FlakeType::Mercurial(_) => "mercurial",
            FlakeType::Tarball(_) => "tarball",
            FlakeType::File(_) => "file",
            FlakeType::Github => "github",
            FlakeType::Gitlab => "gitlab",
            FlakeType::Sourcehut => "sourcehut",
            FlakeType::Indirect => "indirect",
        }
        .to_string()
    }
}

/// The available protocol types for git flake references
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GitProtocolType {
    Http,
    Https,
    Ssh,
    Git,
    File,
    None,
}

impl FromStr for GitProtocolType {
    type Err = UriParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use GitProtocolType::*;
        let ty = match s {
            "http" => Http,
            "https" => Https,
            "ssh" => Ssh,
            "git" => Git,
            "file" => File,
            "" => None,
            _ => Err(UriParseError::InvalidProtocol(s.to_string()))?,
        };
        Ok(ty)
    }
}

/// The available protocol types for mercurial flake references
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MercurialProtocolType {
    Http,
    Https,
    Ssh,
    File,
}

impl FromStr for MercurialProtocolType {
    type Err = UriParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use MercurialProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "ssh" => Ssh,
            "file" => File,
            _ => Err(UriParseError::InvalidProtocol(s.to_string()))?,
        };
        Ok(protocol)
    }
}

/// The available protocol types for tarball flake references
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TarballProtocolType {
    Http,
    Https,
    File,
}

impl FromStr for TarballProtocolType {
    type Err = UriParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TarballProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "file" => File,
            _ => Err(UriParseError::InvalidProtocol(s.to_string()))?,
        };
        Ok(protocol)
    }
}

/// The available protocol types for tarball flake references
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileProtocolType {
    Http,
    Https,
    File,
}

impl FromStr for FileProtocolType {
    type Err = UriParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use FileProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "file" => File,
            _ => Err(UriParseError::InvalidProtocol(s.to_string()))?,
        };
        Ok(protocol)
    }
}

/// Calls the `parser_util` binary with error handling
fn call_bin<T>(bin_path: &Path, flag: &str, flake_ref: &T) -> Result<String, UriParseError>
where
    T: AsRef<str>,
{
    let output = Command::new(bin_path)
        .arg(flag)
        .arg(flake_ref.as_ref())
        .output()?;
    if !output.status.success() {
        let stderr = String::from_utf8(output.stderr)?;
        Err(UriParseError::ParserError(output.status, stderr))
    } else {
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout)
    }
}

/// Resolves and parses a flake reference without checking that the flake being referenced exists.
///
/// If you'd like to check that the flake _does_ exist see [lock_flake_ref].
pub fn resolve_flake_ref<T>(
    flake_ref: T,
    bin_path: &Path,
) -> Result<ResolvedFlakeRef, UriParseError>
where
    T: AsRef<str>,
{
    let output = call_bin(bin_path, "-r", &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // ResolvedFlakeRef::try_from::<()>(generic_parsed_uri) for some reason
    let generic_parsed_uri: GenericParsedURI = serde_json::from_str(output.as_ref())?;
    ResolvedFlakeRef::try_from(generic_parsed_uri)
}

/// Parses and locks a flake reference.
pub fn lock_flake_ref<T>(flake_ref: T, bin_path: &Path) -> Result<LockedFlakeRef, UriParseError>
where
    T: AsRef<str>,
{
    let output = call_bin(bin_path, "-l", &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // LockedFlakeRef::try_from::<()>(generic_parsed_uri) for some reason
    let generic_parsed_uri: GenericParsedURI = serde_json::from_str(output.as_ref())?;
    LockedFlakeRef::try_from(generic_parsed_uri)
}

/// Resolves a flake reference to an installable
pub fn installable_flake_ref<T>(
    flake_ref: T,
    bin_path: &Path,
) -> Result<InstallableFlakeRef, UriParseError>
where
    T: AsRef<str>,
{
    let output = call_bin(bin_path, "-i", &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // InstallableFlakeRef::try_from::<()>(generic_parsed_uri) for some reason
    let generic_parsed_uri: GenericParsedURI = serde_json::from_str(output.as_ref())?;
    InstallableFlakeRef::try_from(generic_parsed_uri)
}

/// Extracts the `narHash` flake attributes
pub(crate) fn extract_nar_hash_attr(attrs: &Attrs) -> Result<Option<NarHash>, UriParseError> {
    let nar_hash = match attrs.get("narHash") {
        Some(Value::String(nar_hash)) => Some(nar_hash.clone()),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "narHash".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(nar_hash)
}

/// Extracts the `rev` flake attribute
pub(crate) fn extract_rev_attr(attrs: &Attrs) -> Result<Option<Rev>, UriParseError> {
    let rev = match attrs.get("rev") {
        Some(Value::String(rev)) => {
            let checked_rev = Rev::from_str(rev)?;
            Some(checked_rev)
        },
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "narHash".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(rev)
}

/// Extracts the `dir` flake attribute
pub(crate) fn extract_dir_attr(attrs: &Attrs) -> Result<Option<PathBuf>, UriParseError> {
    let dir = match attrs.get("dir") {
        Some(Value::String(dir)) => Some(PathBuf::from(dir)),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "dir".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(dir)
}

/// Extracts the `ref` flake attribute
pub(crate) fn extract_ref_attr(attrs: &Attrs) -> Result<Option<String>, UriParseError> {
    let reference = match attrs.get("ref") {
        Some(Value::String(reference)) => Some(reference.clone()),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "ref".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(reference)
}

/// Extracts the `lastModified` flake attribute
pub(crate) fn extract_last_modified_attr(
    attrs: &Attrs,
) -> Result<Option<LastModified>, UriParseError> {
    let last_modified = match attrs.get("lastModified") {
        Some(Value::Number(last_modified)) => {
            let epoch_seconds = last_modified.as_i64();
            if epoch_seconds.is_none() {
                return Err(UriParseError::Other(
                    "'lastModified' did not fit in i64".to_string(),
                ));
            }
            let ts_deser = TimestampDeserialize::TsI64(epoch_seconds.unwrap());
            let ts = Timestamp::try_from(ts_deser)?;
            Some(ts)
        },
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "narHash".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(last_modified)
}

/// Extracts the 'host' flake attribute
pub(crate) fn extract_host_attr(attrs: &Attrs) -> Result<Option<String>, UriParseError> {
    let host = match attrs.get("host") {
        Some(Value::String(host)) => Some(host.clone()),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "host".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(host)
}

/// Extracts the `revCount` flake attribute
pub(crate) fn extract_rev_count_attr(attrs: &Attrs) -> Result<Option<RevCount>, UriParseError> {
    let rev_count = match attrs.get("revCount") {
        Some(Value::Number(rev_count_num)) => {
            let rev_count = rev_count_num.as_u64();
            if rev_count.is_none() {
                return Err(UriParseError::Other(
                    "couldn't convert 'revCount' attribute to u64".to_string(),
                ));
            }
            Some(RevCount::from(rev_count.unwrap())) // already checked that it's safe
        },
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "revCount".to_string(),
                "Number".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(rev_count)
}

/// Extracts the `shallow` flake attribute
pub(crate) fn extract_shallow_attr(attrs: &Attrs) -> Result<Option<bool>, UriParseError> {
    let shallow = match attrs.get("shallow") {
        Some(Value::Bool(shallow)) => Some(shallow),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "shallow".to_string(),
                "Bool".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(shallow.copied())
}

/// Extracts the `submodules` flake attribute
pub(crate) fn extract_submodules_attr(attrs: &Attrs) -> Result<Option<bool>, UriParseError> {
    let submodules = match attrs.get("submodules") {
        Some(Value::Bool(submodules)) => Some(submodules),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "submodules".to_string(),
                "Bool".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(submodules.copied())
}

/// Extracts the `allRefs` flake attribute
pub(crate) fn extract_all_refs_attr(attrs: &Attrs) -> Result<Option<bool>, UriParseError> {
    let all_refs = match attrs.get("allRefs") {
        Some(Value::Bool(all_refs)) => Some(all_refs),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "allRefs".to_string(),
                "Bool".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(all_refs.copied())
}

/// Extracts the `path` flake attribute
pub(crate) fn extract_path_attr(attrs: &Attrs) -> Result<Option<PathBuf>, UriParseError> {
    let path = match attrs.get("path") {
        Some(Value::String(path)) => Some(PathBuf::from(path)),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "path".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(path)
}

/// Extracts the `unpack` flake attribute
pub(crate) fn extract_unpack_attr(attrs: &Attrs) -> Result<Option<bool>, UriParseError> {
    let unpack = match attrs.get("unpack") {
        Some(Value::Bool(unpack)) => Some(unpack),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "unpack".to_string(),
                "Bool".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(unpack.copied())
}

/// Extracts the `name` flake attribute
pub(crate) fn extract_name_attr(attrs: &Attrs) -> Result<Option<String>, UriParseError> {
    let name = match attrs.get("name") {
        Some(Value::String(name)) => Some(name.clone()),
        Some(v) => {
            return Err(UriParseError::AttributeType(
                "name".to_string(),
                "String".to_string(),
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(name)
}

/// Sets the path to the `parser-util` binary.
///
/// This is mostly needed because we call `resolve_flake_ref` from inside `from_str`,
/// which only takes a &str argument so there's no way to pass the binary path to that
/// particular method. Instead we set the path as a global variable (gross, I know) so
/// that it can be looked up inside `from_str`.
pub fn set_parser_util_binary_path(path: Option<PathBuf>) -> Result<(), UriParseError> {
    match path {
        Some(pathbuf) => {
            // This is a Result, but the Ok case means we successfully set the path to
            // the binary, and the Err case means it's _already_ set. In that case we
            // don't really care, we just want to make sure that the path exists.
            //
            // In practice this has only been an error in tests where each test is run
            // concurrently in a separate thread, so multiple tests may be attempting
            // to set the binary path. If you run a single test that sets the binary
            // you don't encounter an error.
            let _result = PARSER_UTIL_BIN_PATH.set(pathbuf);
        },
        None => match std::env::var("PARSER_UTIL_BIN") {
            Ok(string) => {
                let pathbuf = PathBuf::from(string);
                // See the note above about why we ignore this Result
                let _result = PARSER_UTIL_BIN_PATH.set(pathbuf);
            },
            Err(_) => return Err(UriParseError::BinPathNotSet),
        },
    }
    Ok(())
}

#[cfg(test)]
/// Reads the `PARSER_BIN_UTIL` environment variable to get the path to the `parser_util` binary.
pub(crate) fn get_bin() -> PathBuf {
    let string = std::env::var("PARSER_UTIL_BIN").unwrap();
    PathBuf::from(string)
}

#[cfg(test)]
mod test {
    use super::*;

    const RESOLVED_JSON: &str = r#"
    {
        "input": {
          "dir": "lib",
          "id": "nixpkgs",
          "ref": "23.05",
          "type": "indirect"
        },
        "originalRef": {
          "attrs": {
            "dir": "lib",
            "id": "nixpkgs",
            "ref": "23.05",
            "type": "indirect"
          },
          "string": "flake:nixpkgs/23.05?dir=lib"
        },
        "resolvedRef": {
          "attrs": {
            "dir": "lib",
            "owner": "NixOS",
            "ref": "23.05",
            "repo": "nixpkgs",
            "type": "github"
          },
          "string": "github:NixOS/nixpkgs/23.05?dir=lib"
        }
    }
    "#;

    const DUMMY_BIN_PATH: &str = "/path/to/binary";

    /// Converts JSON values into strings
    pub(crate) fn json2string(value: &Value) -> String {
        if let Value::String(string) = value {
            string.clone()
        } else {
            value.to_string()
        }
    }

    #[test]
    fn sets_and_gets_binary_path_from_env() {
        // We know that the PARSER_UTIL_BIN environment variable is set by the Nix shell
        // so we should get the same path reading the environment variable directly and
        // reading from the global variable (once it's been set).
        let path_from_env = get_bin();
        set_parser_util_binary_path(None).unwrap();
        let global_path = PARSER_UTIL_BIN_PATH.get().unwrap();
        // There's a race condition due to tests running concurrently, so we check that
        // we either get the path in the environment or the path that was set in another
        // test
        let expected_path_was_set =
            (path_from_env == *global_path) || (PathBuf::from(DUMMY_BIN_PATH) == *global_path);
        assert!(expected_path_was_set);
    }

    #[test]
    fn sets_and_gets_binary_path_directly() {
        let path = PathBuf::from(DUMMY_BIN_PATH);
        set_parser_util_binary_path(Some(path.clone())).unwrap();
        let global_path = PARSER_UTIL_BIN_PATH.get().unwrap();
        // There's a race condition due to tests running concurrently, so we check that
        // we either get the path in the environment or the path that was set in another
        // test
        let path_from_env = get_bin();
        let expected_path_was_set = (path == *global_path) || (path_from_env == *global_path);
        assert!(expected_path_was_set);
    }

    #[test]
    fn deserializes_at_all() {
        let _: GenericParsedURI = serde_json::from_str(RESOLVED_JSON).unwrap();
    }

    #[test]
    fn resolves_flake_ref() {
        let parsed: GenericParsedURI = serde_json::from_str(RESOLVED_JSON).unwrap();
        let _: ResolvedFlakeRef = ResolvedFlakeRef::try_from(parsed).unwrap();
    }

    #[test]
    fn parses_binary_output() {
        let bin_path = get_bin();
        let _parsed = resolve_flake_ref("github:flox/flox", bin_path.as_ref()).unwrap();
    }

    fn fix_test_bank_path(path: &str) -> String {
        let current_dir = std::env::current_dir().unwrap();
        let dir_str = current_dir.to_str().unwrap();
        path.replace("/tmp/parser-util-test-root", dir_str)
    }

    #[test]
    fn parses_test_bank() {
        let bin_path = get_bin();
        let test_bank_path = PathBuf::from(env!("PARSER_UTIL_TEST_BANK"));
        let f = std::fs::File::open(test_bank_path).unwrap();
        let parsed_test_bank: Value = serde_json::from_reader(f).unwrap();
        let Value::Array(test_cases) = parsed_test_bank else {panic!("wasn't an array of test cases");};
        for test_case in test_cases.iter() {
            // All of this is digging down into the parsed JSON test bank to get the "input"
            // and "originalRef" attributes for this test case
            let Value::Object(tc) = test_case else {panic!("test case wasn't an object")};
            let Some(Value::String(input)) = tc.get("input") else {panic!("missing 'input' attribute")};
            let Some(Value::Object(raw_original_ref)) = tc.get("originalRef") else {panic!("missing 'originalRef'")};
            let resolved_flake_ref = resolve_flake_ref(input, bin_path.as_ref()).unwrap();
            let Some(Value::Object(attrs)) = raw_original_ref.get("attrs") else {panic!("missing 'attrs' attribute")};
            let parsed_ref = resolved_flake_ref.original_ref;
            // This is comparing all of the attributes of the parsed flake reference and the parsed test case
            // regardless of whether we actually use them in our code. This wrapper of the URI parser is
            // intended to be a faithful representation of _everything_ that the Nix URI parser returns to us,
            // not just the parts we use in `runix`.
            for (attr, value) in attrs.iter() {
                if attr == "type" {
                    // The parsed "type" attribute isn't stored as a string
                    // so we need to convert it back into one
                    let type_string: String = parsed_ref.flake_type.into();
                    assert_eq!(
                        json2string(value),
                        type_string,
                        "types didn't match for input '{}'",
                        input
                    );
                } else if attr == "path" {
                    // The test bank resolved all "." paths to "/tmp/parser-util-test-root"
                    // when it was created so we need to replace "." with the _actual_
                    // current directory
                    let Value::String(original_path) = value else {panic!("'path' attr wasn't a string")};
                    let fixed_path = fix_test_bank_path(original_path);
                    let parsed_attr_value = parsed_ref
                        .attrs
                        .get(attr)
                        .unwrap_or_else(|| panic!("missing attr {}", attr));
                    let parsed_attr_str = json2string(parsed_attr_value);
                    assert_eq!(
                        parsed_attr_str, fixed_path,
                        "attr 'path' didn't match for input '{}'",
                        input
                    );
                } else {
                    // All other attributes are stored as serde_json::Value in both
                    // the parsed flake reference and the parsed test bank, so we
                    // don't need special handling for these attributes
                    let parsed_attr_value = parsed_ref
                        .attrs
                        .get(attr)
                        .unwrap_or_else(|| panic!("missing attr {}", attr));
                    assert_eq!(
                        parsed_attr_value, value,
                        "attr '{}' didn't match for input '{}'",
                        attr, input
                    );
                }
            }
        }
    }
}
