//! All that appertains to command-line parsing.

use clap::builder::PossibleValue;
use clap::{arg, command, value_parser, Command, ValueEnum};
use std::ffi::OsString;
use std::fmt::{self, Display, Formatter};
use std::stringify;
use termcolor::ColorChoice;

/// Contains code related to "feature list" option parsing.
pub mod features {
    use clap::{
        builder::{TypedValueParser, ValueParserFactory},
        Arg, Command, ErrorKind, PossibleValue,
    };
    use paste::paste;
    use std::ffi::OsStr;
    use std::marker::PhantomData;

    /// Common trait for "features" whose display can be toggled on and off from a keyword list.
    pub trait Features {
        /// A list of all the keywords that can be accepted; one per feature.
        const KEYWORDS: &'static [&'static str];
        const DEFAULT: &'static str;

        /// Create a new, empty, set of features.
        fn new() -> Self;

        /// Enable or disable all features at once; used by the pseudo-keywords `all` and `none`.
        ///
        /// Exception: features prefixed with a `-` are *not* enabled by this.
        fn set_all(&mut self, enable: bool);
        /// Enable or disable a single feature.
        fn set(&mut self, which: usize, enable: bool);

        /// Is any feature enabled?
        fn any(&self) -> bool;
        /// Is a given feature enabled?
        fn get(&self, which: usize) -> bool;
    }

    /// CLI parser for [Features] "keyword lists".
    #[derive(Debug, Clone)]
    pub struct FeaturesParser<F: Features + Clone + Send + Sync> {
        result_type: PhantomData<F>,
    }

    impl<F: Features + Clone + Send + Sync> FeaturesParser<F> {
        /// Creates a new [Features] "keyword list" parser.
        pub fn new() -> Self {
            Self {
                result_type: PhantomData,
            }
        }
    }

    impl<F: 'static + Features + Clone + Send + Sync> TypedValueParser for FeaturesParser<F> {
        type Value = F;

        fn parse_ref(
            &self,
            _cmd: &Command<'_>,
            _arg: Option<&Arg<'_>>,
            value: &OsStr,
        ) -> Result<<Self as TypedValueParser>::Value, clap::Error> {
            let mut features = F::new();
            let value = value.to_str().ok_or_else(|| {
                clap::Error::raw(ErrorKind::InvalidUtf8, "Invalid UTF-8 in argument")
            })?;

            for keyword in value.split(',') {
                let keyword = keyword.trim();

                // An initial dash prefix negates the keyword
                let (keyword, enable) = if let Some('-') = keyword.chars().next() {
                    (&keyword[1..], false)
                } else {
                    (keyword, true)
                };

                if keyword.is_empty() {
                    return Err(clap::Error::raw(
                        ErrorKind::InvalidValue,
                        "Empty keyword in list",
                    ));
                }

                if keyword == "all" {
                    features.set_all(enable);
                } else if keyword == "none" {
                    features.set_all(!enable);
                } else {
                    // See which keyword this matches; if none, report the error
                    let which = F::KEYWORDS
                        .iter()
                        .enumerate()
                        .find(|(_, &candidate)| keyword == candidate)
                        .ok_or_else(|| {
                            let mut msg = format!(
                                "Unknown keyword \"{}\", expected one of: all, none",
                                keyword
                            );
                            for keyword in F::KEYWORDS {
                                msg.push_str(", ");
                                msg.push_str(keyword);
                            }
                            clap::Error::raw(ErrorKind::InvalidValue, msg)
                        })?
                        .0;
                    features.set(which, enable);
                }
            }

            Ok(features)
        }

        fn possible_values(&self) -> Option<Box<dyn Iterator<Item = PossibleValue<'static>> + '_>> {
            Some(Box::new(
                F::KEYWORDS
                    .iter()
                    .map(|keyword| PossibleValue::new(keyword)),
            ))
        }
    }

    // This pile of macros avoids using a proc macro, which would require a separate crate.
    // The proc macro would likely be less code, but not simpler(?)
    // (This is largely made possible by the [paste] macro)

    macro_rules! enum_def {
        ($name:ident: $($(+)? $(-)? $keyword:ident),*) => {
            paste! {
                #[doc = "An auto-generated list of bit numbers for [" $name "]'s various features."]
                enum [< $name Values >] { $([< $keyword:camel >]),* }

                impl $name {
                    $(pub const [< $keyword:snake:upper >] : usize = [< $name Values >]::[< $keyword:camel >] as usize;)*
                }
            }
        };
    }
    macro_rules! default_val {
        ($(,)?) => {0};
        ($(-)? $keyword:ident $(, $($tail:tt)*)?) => { default_val!($($($tail)*)?) };
        (+ $keyword:ident $(, $($tail:tt)*)?) => { 1 << paste! { Self::[< $keyword:snake:upper >] } | default_val!($($($tail)*)?) };
    }
    // This extracts all identifiers preceded by `+` and their matching comma (except for the first one, which has none)
    // ...don't stare at for too long.
    macro_rules! default_str {
        ($($(-)? $ldummy:ident),* $(,)? $(
            + $first:ident $(, $(-)? $rdummy:ident)* $(, + $keyword:ident $(, $(-)? $tail:ident)*)*
        )?) => { stringify!($($first $(, $keyword)*)?) };
    }
    macro_rules! keywords {
        ($($(+)? $(-)? $keyword:ident),*) => {
            &[ $(stringify!($keyword)),* ]
        };
    }
    macro_rules! all {
        ($(,)?) => {0};
        ($(+)? $keyword:ident $(, $($tail:tt)*)?) => { 1 << paste! { Self::[< $keyword:snake:upper >] } | all!($($($tail)*)?) };
        (- $keyword:ident $(, $($tail:tt)*)?) => { all!($($($tail)*)?) };
    }
    /// Generates a collection that implements [Features] and can be parsed.
    ///
    /// Syntax: (name: feature1, feature2, +feature3, -feature4, ...)
    ///
    /// Each feature must be an identifier or a keyword.
    /// Prefixing a feature with a `+` makes it enabled by default, whereas prefixing it with `-` prevents the pseudo-feature `all` from enabling it.
    macro_rules! features {
        ($name:ident: $($args:tt)*) => {
            /// An auto-generated collection of [Features].
            #[derive(Debug, Clone, Copy)]
            pub struct $name(u8);

            enum_def!{$name: $($args)*}

            impl Default for $name {
                fn default() -> Self {
                    Self(default_val!($($args)*))
                }
            }

            impl Features for $name {
                const KEYWORDS: &'static [&'static str] = keywords!($($args)*);
                const DEFAULT: &'static str = default_str!($($args)*);

                fn new() -> Self {
                    Self(0)
                }
                fn set_all(&mut self, enable: bool) {
                    if enable {
                        self.0 |= all!($($args)*);
                    } else {
                        self.0 = 0;
                    }
                }

                fn set(&mut self, which: usize, enable: bool) {
                    if enable {
                        self.0 |= 1 << which;
                    } else {
                        self.0 &= !(1 << which);
                    }
                }

                fn any(&self) -> bool {
                    self.0 != 0
                }

                fn get(&self, which: usize) -> bool {
                    (self.0 >> which) & 1 != 0
                }
            }

            impl ValueParserFactory for $name {
                type Parser = FeaturesParser<$name>;

                fn value_parser() -> Self::Parser {
                    Self::Parser::new()
                }
            }
        };
    }

    // Implementations

    features!(HeaderFeatures: +size, counts);
    features!(SymbolFeatures: +name, type, src, section, value);
    features!(SectionFeatures: +name, size, type, org, bank, align, data);
    features!(PatchFeatures: +count, src, offset, pcsection, pcoffset, type, rpn, data);
    features!(AssertionFeatures: +src, +offset, section, pcoffset, type, rpn, -data, +message);
}
use features::*;

/// Whether color should be enabled for output.
#[derive(Debug, Clone)]
enum Colorization {
    Auto,
    Always,
    Never,
}
impl ValueEnum for Colorization {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Auto, Self::Always, Self::Never]
    }
    fn to_possible_value<'a>(&self) -> Option<PossibleValue<'a>> {
        Some(PossibleValue::new(match self {
            Self::Auto => "auto",
            Self::Always => "always",
            Self::Never => "never",
        }))
    }
}

/// How RPN expressions should be printed.
#[derive(Debug, Clone, Copy)]
pub enum RpnPrintType {
    Expr,
    Infix,
}

impl RpnPrintType {
    fn name(&self) -> &'static str {
        match self {
            Self::Expr => "RPN",
            Self::Infix => "infix",
        }
    }
}

impl Display for RpnPrintType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl ValueEnum for RpnPrintType {
    fn value_variants<'a>() -> &'a [Self] {
        &[Self::Expr, Self::Infix]
    }
    fn to_possible_value<'a>(&self) -> Option<PossibleValue<'a>> {
        Some(PossibleValue::new(self.name()))
    }
}

fn get_cmd() -> Command<'static> {
    command!()
        .about("Prints out RGBDS object files in a human-friendly manner")
        .after_help("A keyword list is a comma-separated list of keywords.\nA keyword can be prefixed with a dash '-', negating its effect.\nThe special keywords 'all' and 'none' enable or disable all keywords unless specified otherwise.\n\nSee `man 1 rgbobj` for more information, including what each keyword does.")
        .arg(arg!(-A --all "Display \"all\" output for all output types; this overrides other display options"))
        .arg(arg!(--color <enable> "Whether to color output").value_parser(value_parser!(Colorization)).default_value("auto").required(false))
        .arg(arg!(-r --rpn <format> "The format to use for printing RPN").value_parser(value_parser!(RpnPrintType)).default_value("RPN").required(false))
        .arg(arg!(-h --header <features> "Keyword list of what to display about the header").value_parser(value_parser!(HeaderFeatures)).default_value(HeaderFeatures::DEFAULT).required(false))
        .arg(arg!(-y --symbol <features> "Keyword list of what to display about symbols").value_parser(value_parser!(SymbolFeatures)).default_value(SymbolFeatures::DEFAULT).required(false))
        .arg(arg!(-s --section <features> "Keyword list of what to display about sections").value_parser(value_parser!(SectionFeatures)).default_value(SectionFeatures::DEFAULT).required(false))
        .arg(arg!(-p --patch <features> "Keyword list of what to display about patches").value_parser(value_parser!(PatchFeatures)).default_value(PatchFeatures::DEFAULT).required(false))
        .arg(arg!(-a --assertion <features> "Keyword list of what to display about assertions").value_parser(value_parser!(AssertionFeatures)).default_value(AssertionFeatures::DEFAULT).required(false))
        .arg(arg!(<file> "Path to the object file to inspect"))
}

/// Parse command-line arguments.
pub fn parse() -> Args {
    let matches = get_cmd().get_matches();

    let (color_out, color_err) = match matches.get_one("color").unwrap() {
        Colorization::Always => (ColorChoice::Always, ColorChoice::Always),
        Colorization::Never => (ColorChoice::Never, ColorChoice::Never),
        Colorization::Auto => {
            use atty::Stream::*;
            let auto = |stream| {
                if atty::is(stream) {
                    ColorChoice::Auto
                } else {
                    ColorChoice::Never
                }
            };
            (auto(Stdout), auto(Stderr))
        }
    };

    let mut header: HeaderFeatures = matches
        .get_one("header")
        .map_or_else(HeaderFeatures::default, |val| *val);
    let mut symbol: SymbolFeatures = matches
        .get_one("symbol")
        .map_or_else(SymbolFeatures::default, |val| *val);
    let mut section: SectionFeatures = matches
        .get_one("section")
        .map_or_else(SectionFeatures::default, |val| *val);
    let mut patch: PatchFeatures = matches
        .get_one("patch")
        .map_or_else(PatchFeatures::default, |val| *val);
    let mut assertion: AssertionFeatures = matches
        .get_one("assertion")
        .map_or_else(AssertionFeatures::default, |val| *val);
    if matches.is_present("all") {
        header.set_all(true);
        symbol.set_all(true);
        section.set_all(true);
        patch.set_all(true);
        assertion.set_all(true);
    }

    Args {
        rpn: *matches.get_one("rpn").unwrap(),

        header,
        symbol,
        section,
        patch,
        assertion,

        path: matches.value_of("file").unwrap().into(),

        color_out,
        color_err,
    }
}

/// Configuration specified on the command-line.
#[derive(Debug)]
pub struct Args {
    pub rpn: RpnPrintType,

    pub header: HeaderFeatures,
    pub symbol: SymbolFeatures,
    pub section: SectionFeatures,
    pub patch: PatchFeatures,
    pub assertion: AssertionFeatures,

    pub path: OsString,

    pub color_out: ColorChoice,
    pub color_err: ColorChoice,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cli_ok() {
        get_cmd().debug_assert();
    }
}
