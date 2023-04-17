//! A much simplified installable representation

use std::fmt::Display;
use std::str::FromStr;

use derive_more::{AsRef, IntoIterator};
use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

use crate::flake_ref::{FlakeRef, ParseFlakeRefError};

/// regex listing valid characters for attributes
///
/// derived from https://github.com/NixOS/nix/blob/master/src/libutil/url-parts.hh
static VALID_ATTRIBUTE: Lazy<Regex> =
    Lazy::new(|| Regex::new("^([a-zA-Z0-9-._~!$&'()*+,;=:%@?/ ]*)$").unwrap());

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
#[derive(Debug, Clone, PartialEq, Eq, Default, IntoIterator)]
pub struct AttrPath(Vec<Attribute>);

impl AttrPath {
    /// Add another component to the end of the attrpath
    pub fn push_attr(&mut self, attr: &str) -> Result<&mut Self, <Attribute as FromStr>::Err> {
        self.0.push(attr.parse::<Attribute>()?);
        Ok(self)
    }

    /// get an iterator over all components of the attrpath
    pub fn iter(&self) -> impl Iterator<Item = &Attribute> {
        self.0.iter()
    }

    /// get an iterator over all components of the attrpath and allow modification
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Attribute> {
        self.0.iter_mut()
    }

    /// determine whether the attrpath is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromIterator<Attribute> for AttrPath {
    fn from_iter<T: IntoIterator<Item = Attribute>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl FromStr for AttrPath {
    type Err = ParseInstallableError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = percent_encoding::percent_decode_str(s).decode_utf8()?;

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
            return Err(ParseInstallableError::UnclosedQuote(s[start..].to_string()));
        }

        if !cur.is_empty() {
            attributes.push_attr(&cur)?;
        }
        Ok(attributes)
    }
}

impl Display for AttrPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (n, attr) in self.iter().enumerate() {
            if n > 0 {
                write!(f, ".")?;
            }
            write!(
                f,
                "{}",
                percent_encoding::utf8_percent_encode(
                    &attr.to_string(),
                    percent_encoding::CONTROLS
                )
            )?;
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
#[derive(Debug, Clone, PartialEq, Eq, AsRef)]
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
    #[error("Invalid attrpath encoding: {0}")]
    FragmentEncoding(#[from] std::str::Utf8Error),
    #[error("Missing closing quote in selection path: '{0}'")]
    UnclosedQuote(String),
    #[error("Invalid attribute '{0}'")]
    InvalidAttr(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_parse_as(input: &str, expected: &str, description: &str) {
        let output = input.parse::<AttrPath>().expect(description);
        assert_eq!(output.to_string(), expected);
    }

    fn assert_parse(input: &str, description: &str) {
        let output = input.parse::<AttrPath>().expect(description);
        assert_eq!(output.to_string(), input);
    }

    fn assert_parse_compontents<'a>(
        components: impl AsRef<[&'a str]>,
        expect: &str,
        description: &str,
    ) {
        let output = AttrPath::try_from(components.as_ref()).expect(description);
        assert_eq!(output.to_string(), expect);
    }

    fn assert_fail(input: &str, description: &str) {
        input
            .parse::<AttrPath>()
            .expect_err(&format!("({input}) was expected to fail: {description}"));
    }

    #[test]
    fn attr_path_from_str() {
        assert_parse("a", "parse single attribute");
        assert_parse("a.b.c", "parse nested path");
        assert_parse_as("\"a\"", "a", "parse quoted single");
        assert_parse("''a''", "parse '' quoted single");
        assert_parse("2", "parse number");
        assert_parse("\"a.b\"", "parse quoted dot");

        assert_fail("\"${abc}\"", "interpolated");
        assert_fail("a.\"${asdf}\".c", "interpolated inside");
        assert_fail("a.${asdf}.c", "interpolated inside no quotes");
        assert_fail("x.\"open.no.close", "missing quote");
    }

    #[test]
    fn attr_path_from_list() {
        assert_parse_compontents(["a"], "a", "parse single attribute");
        assert_parse_compontents([""], "", "parse empty");
        assert_parse_compontents(["a", "b", "c"], "a.b.c", "parse nested path");
        assert_parse_compontents(["a.b"], "\"a.b\"", "should parse quoted single attribute");

        AttrPath::try_from(["a", "${asdf}", "c"]).expect_err("should not parse with interpolation");
        AttrPath::try_from(["\"${asdf}\"", ".c"])
            .expect_err("should not parse with interpolation in the front");
        AttrPath::try_from(["x.${asdf}", "c"]).expect_err("should not parse with dynamic element");
    }
}
