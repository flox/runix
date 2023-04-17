//! A much simplified installable representation

use std::fmt::Display;
use std::str::FromStr;

use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

use crate::flake_ref::{FlakeRef, ParseFlakeRefError};

/// regex describing valid characters for attributes
/// derived from <https://github.com/flox/nix/blob/ysndr/disable_attrpath_resolution/src/libutil/url-parts.hh#L21>
static VALID_ATTRIBUTE: Lazy<Regex> =
    Lazy::new(|| Regex::new("^([a-zA-Z0-9-._~!$&'()*+,;=:@?/ ]*)$").unwrap());

/// A simplified installable representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Installable {
    pub flakeref: FlakeRef,
    pub attr_path: AttrPath,
}

/// The attrpath component of an installable
///
/// This implementation wraps a [Vec<String>] for components.
/// Its [FromStr] and [Display] implementations are used to parse and validate,
/// as well as canonically print an attrpath.
///
/// The implementation also includes [TryFrom] implementations to allow
/// ergonomic validated conversion from list types
///
/// ```
/// use runix::installable::AttrPath;
///
/// "abc.xyz".parse::<AttrPath>().expect("Parses from String");
/// AttrPath::try_from(["abc", "xyz"]).expect("Parses from array");
/// AttrPath::try_from(&*vec!["abc", "xyz"]).expect("Parses from vec");
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct AttrPath(Vec<Attribute>);

impl AttrPath {
    /// Add another component to the end of the attrpath
    pub fn push_attr(&mut self, attr: &str) -> Result<&mut Self, <Attribute as FromStr>::Err> {
        self.0.push(attr.parse::<Attribute>()?);
        Ok(self)
    }

    /// get an iterator over all components of the attrpath
    pub fn components(&self) -> impl Iterator<Item = &Attribute> {
        self.0.iter()
    }

    /// determine whether the attrpath is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromStr for AttrPath {
    type Err = ParseInstallableError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut attributes = AttrPath::default();
        let mut cur = String::new();

        let mut start_quote = None;

        for (n, c) in s.chars().enumerate() {
            match c {
                '.' if start_quote.is_none() => {
                    attributes.push_attr(&std::mem::take(&mut cur))?;
                },
                '"' if start_quote.is_some() => start_quote = None,
                '"' if start_quote.is_none() => start_quote = Some(n),
                other => cur.push(other),
            }
        }

        if let Some(start) = start_quote {
            return Err(ParseInstallableError::UnclosedQuote(
                s.to_string().split_off(start),
            ));
        }

        if !cur.is_empty() {
            attributes.push_attr(&cur)?;
        }
        Ok(attributes)
    }
}

impl Display for AttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((first, rest)) = self.0.split_first() {
            write!(f, "{first}")?;
            for attr in rest {
                write!(f, ".{attr}")?;
            }
        }
        Ok(())
    }
}

impl<A: AsRef<str>> TryFrom<&[A]> for AttrPath {
    type Error = ParseInstallableError;

    fn try_from(value: &[A]) -> Result<Self, Self::Error> {
        value
            .iter()
            .map(|a| a.as_ref().parse())
            .collect::<Result<_, _>>()
            .map(Self)
    }
}

impl<A: AsRef<str>, const N: usize> TryFrom<[A; N]> for AttrPath {
    type Error = ParseInstallableError;

    fn try_from(value: [A; N]) -> Result<Self, Self::Error> {
        value.as_slice().try_into()
    }
}

/// A validated attribute
///
/// Component of an attrpath
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute(String);

impl FromStr for Attribute {
    type Err = ParseInstallableError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !(VALID_ATTRIBUTE.is_match(s)) {
            Err(ParseInstallableError::InvalidAttr(s.to_string()))?;
        }
        Ok(Attribute(s.to_string()))
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.contains('.') {
            write!(f, "\"{}\"", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl FromStr for Installable {
    type Err = ParseInstallableError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_once('#') {
            Some((flakeref, attr_path)) => Ok(Installable {
                flakeref: flakeref.parse()?,
                attr_path: attr_path.parse()?,
            }),
            None => Ok(Installable {
                flakeref: s.parse()?,
                attr_path: "".parse()?,
            }),
        }
    }
}

impl Display for Installable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.flakeref)?;
        if !self.attr_path.is_empty() {
            write!(f, "#{}", self.attr_path)?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum ParseInstallableError {
    #[error(transparent)]
    ParseFlakeRef(#[from] ParseFlakeRefError),
    #[error("Installable is missing an attribute path")]
    MissingAttrPath,
    #[error(
        "Missing closing quote in selection path: '
    {0}'"
    )]
    UnclosedQuote(String),
    #[error("Invalid attribute '{0}'")]
    InvalidAttr(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attr_path_from_str() {
        "a".parse::<AttrPath>().expect("should parse single attribute");
        "a.b.c"
            .parse::<AttrPath>()
            .expect("should parse nested path");
        "\"a\""
            .parse::<AttrPath>()
            .expect("should parse quoted single");

        "''a''"
            .parse::<AttrPath>()
            .expect("should parse '' quoted single");
        "\"${abc}\""
            .parse::<AttrPath>()
            .expect_err("should not parse just interpolated");
        "a.\"${asdf}\".c"
            .parse::<AttrPath>()
            .expect_err("should not parse with interpolation");
        "a.\"${\"asdf\"}\".c"
            .parse::<AttrPath>()
            .expect_err("should not parse with interpolation that interpolates a string");
        "\"${asdf}\".c"
            .parse::<AttrPath>()
            .expect_err("should not parse with interpolation in the front");
        "x.${asdf}.c"
            .parse::<AttrPath>()
            .expect_err("should not parse with dynamic element");

        "x.\"open.no.close"
            .parse::<AttrPath>()
            .expect_err("should detect unclosed");

        "2".parse::<AttrPath>().expect("should parse number");
        "\"2\""
            .parse::<AttrPath>()
            .expect("should parse quoted number");
    }

    #[test]
    fn attr_path_from_list() {
        AttrPath::try_from(["a"]).expect("should parse single attribute");
        AttrPath::try_from([""]).expect("should parse single attribute");
        AttrPath::try_from(["a.b"]).expect("should parse quoted single attribute");
        AttrPath::try_from(["a", "b", "c"]).expect("should parse nested path");
        AttrPath::try_from(["a", "${asdf}", "c"]).expect_err("should not parse with interpolation");
        AttrPath::try_from(["\"${asdf}\"", ".c"])
            .expect_err("should not parse with interpolation in the front");
        AttrPath::try_from(["x.${asdf}", "c"]).expect_err("should not parse with dynamic element");

        dbg!(AttrPath::try_from(["a", "b", "c"])
            .unwrap()
            .components()
            .collect::<Vec<_>>());

        println!("{}", AttrPath::try_from(["a", "b", "c"]).unwrap());

        println!(
            "{}",
            AttrPath::try_from(["a(.)"]).expect("should parse single attribute")
        );
    }
}
