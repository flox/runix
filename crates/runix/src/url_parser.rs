use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};
use std::str::FromStr;
use std::string::FromUtf8Error;

use serde::de::Error;
use serde::{Deserialize, Deserializer};
use serde_json::Value;

use crate::flake_ref::lock::{InvalidRev, LastModified, NarHash, Rev, RevCount};
use crate::flake_ref::protocol::WrappedUrlParseError;
use crate::flake_ref::{ParseTimeError, Timestamp, TimestampDeserialize};

pub static PARSER_UTIL_BIN_PATH: &str = env!("PARSER_UTIL_BIN");

/// The various errors that can be encountered parsing the JSON output of `parser-util`.
///
/// Some of these are legitimate errors that can occur deserializing the JSON to structured
/// types. Others are error cases that we inflict upon ourselves trying to strongly type
/// the parsed data we get back from Nix/`parser-util`.
#[derive(Debug, thiserror::Error)]
pub enum UrlParseError {
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
    #[error("parsed URL was missing attribute '{0}'")]
    MissingAttribute(&'static str),
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
    AttributeType(&'static str, &'static str, Value),
    #[error("{0}")]
    Other(String),
}

/// A flake reference URL that has been resolved by Nix, but has not been
/// checked that it exists i.e. locked.
#[derive(Debug, Clone)]
pub struct ResolvedFlakeRef {
    pub input: Input,
    pub original_ref: ParsedFlakeReference,
    pub resolved_ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURL> for ResolvedFlakeRef {
    type Error = UrlParseError;

    fn try_from(parsed: GenericParsedURL) -> Result<Self, Self::Error> {
        let input = parsed
            .input
            .ok_or_else(|| UrlParseError::MissingAttribute("input"))?;
        let original_ref = parsed
            .original_ref
            .ok_or_else(|| UrlParseError::MissingAttribute("originalRef"))
            .and_then(ParsedFlakeReference::try_from)?;
        let resolved_ref = parsed
            .resolved_ref
            .ok_or_else(|| UrlParseError::MissingAttribute("resolvedRef"))
            .and_then(ParsedFlakeReference::try_from)?;

        Ok(ResolvedFlakeRef {
            input,
            original_ref,
            resolved_ref,
        })
    }
}

/// A flake reference URL that has been resolved and checked by Nix that it exists
/// i.e. locked.
#[derive(Debug, Clone)]
pub struct LockedFlakeRef {
    pub input: Input,
    pub original_ref: ParsedFlakeReference,
    pub resolved_ref: ParsedFlakeReference,
    pub locked_ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURL> for LockedFlakeRef {
    type Error = UrlParseError;

    fn try_from(mut parsed: GenericParsedURL) -> Result<Self, Self::Error> {
        let locked_ref = parsed
            .locked_ref
            .take()
            .ok_or_else(|| UrlParseError::MissingAttribute("lockedRef"))
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

/// A flake reference URL pointing to an installable that has been resolved by Nix
#[derive(Debug, Clone)]
pub struct InstallableFlakeRef {
    pub input: Input,
    pub attr_path: Vec<String>,
    pub outputs: InstallableOutputs,
    pub r#ref: ParsedFlakeReference,
}

impl TryFrom<GenericParsedURL> for InstallableFlakeRef {
    type Error = UrlParseError;

    fn try_from(parsed: GenericParsedURL) -> Result<Self, Self::Error> {
        let input = parsed
            .input
            .ok_or_else(|| UrlParseError::MissingAttribute("input"))?;

        let attr_path = parsed
            .attr_path
            .ok_or_else(|| UrlParseError::MissingAttribute("attrPath"))?;

        let outputs = parsed
            .outputs
            .ok_or_else(|| UrlParseError::MissingAttribute("outputs"))?;
        let r#ref = parsed
            .r#ref
            .ok_or_else(|| UrlParseError::MissingAttribute("ref"))
            .and_then(ParsedFlakeReference::try_from)?;

        Ok(InstallableFlakeRef {
            input,
            attr_path,
            outputs,
            r#ref,
        })
    }
}

/// A completely generic parsed URL i.e. a type that can represent a call to `parser_util`
/// with any of the flags
#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "camelCase")]
#[allow(dead_code)]
struct GenericParsedURL {
    #[serde(deserialize_with = "deserialize_input")]
    input: Option<Input>,
    original_ref: Option<DeserializedFlakeRef>,
    resolved_ref: Option<DeserializedFlakeRef>,
    locked_ref: Option<DeserializedFlakeRef>,
    r#ref: Option<DeserializedFlakeRef>,
    attr_path: Option<Vec<String>>,
    outputs: Option<InstallableOutputs>,
    authority: Option<String>,
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

/// The type and contents of the input the URL parser received.
#[derive(Debug, Deserialize, Clone)]
pub enum Input {
    // A flake reference supplied as a URL
    URL(String),
    // A flake reference supplied as an attribute set
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
        String(string) => Ok(Some(Input::URL(string))),
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
#[derive(Debug, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum InstallableOutputs {
    All,
    Default,
    Selected(Vec<String>),
}

impl InstallableOutputs {
    pub fn as_url_suffix(&self) -> String {
        match self {
            InstallableOutputs::Default => String::new(),
            InstallableOutputs::All => "^*".to_string(),
            InstallableOutputs::Selected(ref outputs) => format!("^{}", outputs.join(",")),
        }
    }
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
    type Error = UrlParseError;

    fn try_from(flake: DeserializedFlakeRef) -> Result<Self, Self::Error> {
        use FlakeType::*;
        let Some(raw_flake_type) = flake.attrs.get("type") else {
            return Err(UrlParseError::TypeNotFound);
        };
        let Value::String(raw_flake_type) = raw_flake_type else {
            return Err(UrlParseError::TypeWasWrongType(raw_flake_type.clone()));
        };
        let protocol = url_scheme(flake.attrs.get("url"));
        match raw_flake_type.as_ref() {
            "git" | "mercurial" | "tarball" | "file" => {
                if protocol.is_none() {
                    return Err(UrlParseError::ProtocolNotFound);
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
            _ => return Err(UrlParseError::UnrecognizedFlakeType(raw_flake_type.clone())),
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
    type Err = UrlParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use GitProtocolType::*;
        let ty = match s {
            "http" => Http,
            "https" => Https,
            "ssh" => Ssh,
            "git" => Git,
            "file" => File,
            "" => None,
            _ => Err(UrlParseError::InvalidProtocol(s.to_string()))?,
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
    type Err = UrlParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use MercurialProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "ssh" => Ssh,
            "file" => File,
            _ => Err(UrlParseError::InvalidProtocol(s.to_string()))?,
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
    type Err = UrlParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TarballProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "file" => File,
            _ => Err(UrlParseError::InvalidProtocol(s.to_string()))?,
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
    type Err = UrlParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use FileProtocolType::*;
        let protocol = match s {
            "http" => Http,
            "https" => Https,
            "file" => File,
            _ => Err(UrlParseError::InvalidProtocol(s.to_string()))?,
        };
        Ok(protocol)
    }
}

enum ResolverFlag {
    Resolve,
    Lock,
    Installable,
}

impl ResolverFlag {
    const fn as_flag(&self) -> &str {
        match self {
            ResolverFlag::Resolve => "-r",
            ResolverFlag::Lock => "-l",
            ResolverFlag::Installable => "-i",
        }
    }
}

/// Calls the `parser_util` binary with error handling
fn call_bin(
    bin_path: impl AsRef<Path>,
    flag: ResolverFlag,
    flake_ref: impl AsRef<str>,
) -> Result<String, UrlParseError> {
    let output = Command::new(bin_path.as_ref())
        .arg(flag.as_flag())
        .arg(flake_ref.as_ref())
        .output()?;
    if !output.status.success() {
        let stderr = String::from_utf8(output.stderr)?;
        Err(UrlParseError::ParserError(output.status, stderr))
    } else {
        let stdout = String::from_utf8(output.stdout)?;
        Ok(stdout)
    }
}

/// Resolves and parses a flake reference without checking that the flake being referenced exists.
///
/// If you'd like to check that the flake _does_ exist see [lock_flake_ref].
pub fn resolve_flake_ref(
    flake_ref: impl AsRef<str>,
    bin_path: impl AsRef<Path>,
) -> Result<ResolvedFlakeRef, UrlParseError> {
    let output = call_bin(bin_path, ResolverFlag::Resolve, &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // ResolvedFlakeRef::try_from::<()>(generic_parsed_url) for some reason
    let generic_parsed_url: GenericParsedURL = serde_json::from_str(output.as_ref())?;
    ResolvedFlakeRef::try_from(generic_parsed_url)
}

/// Parses and locks a flake reference.
pub fn lock_flake_ref(
    flake_ref: impl AsRef<str>,
    bin_path: impl AsRef<Path>,
) -> Result<LockedFlakeRef, UrlParseError> {
    let output = call_bin(bin_path, ResolverFlag::Lock, &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // LockedFlakeRef::try_from::<()>(generic_parsed_url) for some reason
    let generic_parsed_url: GenericParsedURL = serde_json::from_str(output.as_ref())?;
    LockedFlakeRef::try_from(generic_parsed_url)
}

/// Resolves a flake reference to an installable
pub fn installable_flake_ref(
    flake_ref: impl AsRef<str>,
    bin_path: impl AsRef<Path>,
) -> Result<InstallableFlakeRef, UrlParseError> {
    let output = call_bin(bin_path, ResolverFlag::Installable, &flake_ref)?;
    // Type annotation needed here otherwise it thinks you're doing
    // InstallableFlakeRef::try_from::<()>(generic_parsed_url) for some reason
    let generic_parsed_url: GenericParsedURL = serde_json::from_str(output.as_ref())?;
    InstallableFlakeRef::try_from(generic_parsed_url)
}

/// Extracts the `narHash` flake attributes
pub(crate) fn extract_nar_hash_attr(attrs: &Attrs) -> Result<Option<NarHash>, UrlParseError> {
    let nar_hash = match attrs.get("narHash") {
        Some(Value::String(nar_hash)) => Some(nar_hash.clone()),
        Some(v) => return Err(UrlParseError::AttributeType("narHash", "String", v.clone())),
        None => None,
    };
    Ok(nar_hash)
}

/// Extracts the `rev` flake attribute
pub(crate) fn extract_rev_attr(attrs: &Attrs) -> Result<Option<Rev>, UrlParseError> {
    let rev = match attrs.get("rev") {
        Some(Value::String(rev)) => {
            let checked_rev = Rev::from_str(rev)?;
            Some(checked_rev)
        },
        Some(v) => return Err(UrlParseError::AttributeType("narHash", "String", v.clone())),
        None => None,
    };
    Ok(rev)
}

/// Extracts the `dir` flake attribute
pub(crate) fn extract_dir_attr(attrs: &Attrs) -> Result<Option<PathBuf>, UrlParseError> {
    let dir = match attrs.get("dir") {
        Some(Value::String(dir)) => Some(PathBuf::from(dir)),
        Some(v) => return Err(UrlParseError::AttributeType("dir", "String", v.clone())),
        None => None,
    };
    Ok(dir)
}

/// Extracts the `ref` flake attribute
pub(crate) fn extract_ref_attr(attrs: &Attrs) -> Result<Option<String>, UrlParseError> {
    let reference = match attrs.get("ref") {
        Some(Value::String(reference)) => Some(reference.clone()),
        Some(v) => return Err(UrlParseError::AttributeType("ref", "String", v.clone())),
        None => None,
    };
    Ok(reference)
}

/// Extracts the `lastModified` flake attribute
pub(crate) fn extract_last_modified_attr(
    attrs: &Attrs,
) -> Result<Option<LastModified>, UrlParseError> {
    let last_modified = match attrs.get("lastModified") {
        Some(Value::Number(last_modified)) => {
            let epoch_seconds = last_modified.as_i64();
            if epoch_seconds.is_none() {
                return Err(UrlParseError::Other(
                    "'lastModified' did not fit in i64".to_string(),
                ));
            }
            let ts_deser = TimestampDeserialize::TsI64(epoch_seconds.unwrap());
            let ts = Timestamp::try_from(ts_deser)?;
            Some(ts)
        },
        Some(v) => return Err(UrlParseError::AttributeType("narHash", "String", v.clone())),
        None => None,
    };
    Ok(last_modified)
}

/// Extracts the 'host' flake attribute
pub(crate) fn extract_host_attr(attrs: &Attrs) -> Result<Option<String>, UrlParseError> {
    let host = match attrs.get("host") {
        Some(Value::String(host)) => Some(host.clone()),
        Some(v) => return Err(UrlParseError::AttributeType("host", "String", v.clone())),
        None => None,
    };
    Ok(host)
}

/// Extracts the `revCount` flake attribute
pub(crate) fn extract_rev_count_attr(attrs: &Attrs) -> Result<Option<RevCount>, UrlParseError> {
    let rev_count = match attrs.get("revCount") {
        Some(Value::Number(rev_count_num)) => {
            let rev_count = rev_count_num.as_u64();
            if rev_count.is_none() {
                return Err(UrlParseError::Other(
                    "couldn't convert 'revCount' attribute to u64".to_string(),
                ));
            }
            Some(RevCount::from(rev_count.unwrap())) // already checked that it's safe
        },
        Some(v) => {
            return Err(UrlParseError::AttributeType(
                "revCount",
                "Number",
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(rev_count)
}

/// Extracts the `shallow` flake attribute
pub(crate) fn extract_shallow_attr(attrs: &Attrs) -> Result<Option<bool>, UrlParseError> {
    let shallow = match attrs.get("shallow") {
        Some(Value::Bool(shallow)) => Some(shallow),
        Some(v) => return Err(UrlParseError::AttributeType("shallow", "Bool", v.clone())),
        None => None,
    };
    Ok(shallow.copied())
}

/// Extracts the `submodules` flake attribute
pub(crate) fn extract_submodules_attr(attrs: &Attrs) -> Result<Option<bool>, UrlParseError> {
    let submodules = match attrs.get("submodules") {
        Some(Value::Bool(submodules)) => Some(submodules),
        Some(v) => {
            return Err(UrlParseError::AttributeType(
                "submodules",
                "Bool",
                v.clone(),
            ))
        },
        None => None,
    };
    Ok(submodules.copied())
}

/// Extracts the `allRefs` flake attribute
pub(crate) fn extract_all_refs_attr(attrs: &Attrs) -> Result<Option<bool>, UrlParseError> {
    let all_refs = match attrs.get("allRefs") {
        Some(Value::Bool(all_refs)) => Some(all_refs),
        Some(v) => return Err(UrlParseError::AttributeType("allRefs", "Bool", v.clone())),
        None => None,
    };
    Ok(all_refs.copied())
}

/// Extracts the `path` flake attribute
pub(crate) fn extract_path_attr(attrs: &Attrs) -> Result<Option<PathBuf>, UrlParseError> {
    let path = match attrs.get("path") {
        Some(Value::String(path)) => Some(PathBuf::from(path)),
        Some(v) => return Err(UrlParseError::AttributeType("path", "String", v.clone())),
        None => None,
    };
    Ok(path)
}

/// Extracts the `unpack` flake attribute
pub(crate) fn extract_unpack_attr(attrs: &Attrs) -> Result<Option<bool>, UrlParseError> {
    let unpack = match attrs.get("unpack") {
        Some(Value::Bool(unpack)) => Some(unpack),
        Some(v) => return Err(UrlParseError::AttributeType("unpack", "Bool", v.clone())),
        None => None,
    };
    Ok(unpack.copied())
}

/// Extracts the `name` flake attribute
pub(crate) fn extract_name_attr(attrs: &Attrs) -> Result<Option<String>, UrlParseError> {
    let name = match attrs.get("name") {
        Some(Value::String(name)) => Some(name.clone()),
        Some(v) => return Err(UrlParseError::AttributeType("name", "String", v.clone())),
        None => None,
    };
    Ok(name)
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

    /// Converts JSON values into strings
    pub(crate) fn json2string(value: &Value) -> String {
        if let Value::String(string) = value {
            string.clone()
        } else {
            value.to_string()
        }
    }

    #[test]
    fn deserializes_at_all() {
        let _: GenericParsedURL = serde_json::from_str(RESOLVED_JSON).unwrap();
    }

    #[test]
    fn resolves_flake_ref() {
        let parsed: GenericParsedURL = serde_json::from_str(RESOLVED_JSON).unwrap();
        let _: ResolvedFlakeRef = ResolvedFlakeRef::try_from(parsed).unwrap();
    }

    #[test]
    fn parses_binary_output() {
        let _parsed = resolve_flake_ref("github:flox/flox", PARSER_UTIL_BIN_PATH).unwrap();
    }

    fn fix_test_bank_path(path: &str) -> String {
        let current_dir = std::env::current_dir().unwrap();
        let dir_str = current_dir.to_str().unwrap();
        path.replace("/tmp/parser-util-test-root", dir_str)
    }

    #[test]
    fn parses_test_bank() {
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
            let resolved_flake_ref = resolve_flake_ref(input, PARSER_UTIL_BIN_PATH).unwrap();
            let Some(Value::Object(attrs)) = raw_original_ref.get("attrs") else {panic!("missing 'attrs' attribute")};
            let parsed_ref = resolved_flake_ref.original_ref;
            // This is comparing all of the attributes of the parsed flake reference and the parsed test case
            // regardless of whether we actually use them in our code. This wrapper of the URL parser is
            // intended to be a faithful representation of _everything_ that the Nix URL parser returns to us,
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
