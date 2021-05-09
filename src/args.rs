use clap::clap_app;
use std::ffi::OsString;
use termcolor::ColorChoice;

pub mod features {
    use clap::ArgMatches;

    /// Common trait for "features" whose display can be toggle on and off from a keyword list
    pub trait Features: Default {
        const KEYWORDS: &'static [&'static str];

        fn new() -> Self;

        fn set_all(&mut self, enable: bool);
        fn set(&mut self, which: usize, enable: bool);

        fn any(&self) -> bool;
        fn get(&self, which: usize) -> bool;
    }

    // Implementations

    #[derive(Debug)]
    pub struct HeaderFeatures(u8);
    impl HeaderFeatures {
        pub const SIZE: usize = 0;
        pub const COUNTS: usize = 1;
    }
    impl Default for HeaderFeatures {
        fn default() -> Self {
            Self(1 << Self::COUNTS)
        }
    }
    impl Features for HeaderFeatures {
        const KEYWORDS: &'static [&'static str] = &["size", "counts"];

        fn new() -> Self {
            Self(0)
        }
        fn set_all(&mut self, enable: bool) {
            if enable {
                self.0 |= 0x3;
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

    #[derive(Debug)]
    pub struct SymbolFeatures(u8);
    impl SymbolFeatures {
        pub const NAME: usize = 0;
        pub const TYPE: usize = 1;
        pub const SRC: usize = 2;
        pub const SECTION: usize = 3;
        pub const VALUE: usize = 4;
    }
    impl Default for SymbolFeatures {
        fn default() -> Self {
            Self(1 << Self::NAME)
        }
    }
    impl Features for SymbolFeatures {
        const KEYWORDS: &'static [&'static str] = &["name", "type", "src", "section", "value"];

        fn new() -> Self {
            Self(0)
        }
        fn set_all(&mut self, enable: bool) {
            if enable {
                self.0 |= 0x1F;
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

    #[derive(Debug)]
    pub struct SectionFeatures(u8);
    impl SectionFeatures {
        pub const NAME: usize = 0;
        pub const SIZE: usize = 1;
        pub const TYPE: usize = 2;
        pub const ORG: usize = 3;
        pub const BANK: usize = 4;
        pub const ALIGN: usize = 5;
        pub const DATA: usize = 6;
    }
    impl Default for SectionFeatures {
        fn default() -> Self {
            Self(1 << Self::NAME)
        }
    }
    impl Features for SectionFeatures {
        const KEYWORDS: &'static [&'static str] =
            &["name", "size", "type", "org", "bank", "align", "data"];

        fn new() -> Self {
            Self(0)
        }
        fn set_all(&mut self, enable: bool) {
            // Do not enable data when requesting "all"
            if enable {
                self.0 |= 0x3F;
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

    #[derive(Debug)]
    pub struct PatchFeatures(u8);
    impl PatchFeatures {
        pub const COUNT: usize = 0;
        pub const SRC: usize = 1;
        pub const OFFSET: usize = 2;
        pub const PCSECTION: usize = 3;
        pub const PCOFFSET: usize = 4;
        pub const TYPE: usize = 5;
        pub const RPN: usize = 6;
        pub const DATA: usize = 7;
    }
    impl Default for PatchFeatures {
        fn default() -> Self {
            Self(1 << Self::COUNT)
        }
    }
    impl Features for PatchFeatures {
        const KEYWORDS: &'static [&'static str] = &[
            "count",
            "src",
            "offset",
            "pcsection",
            "pcoffset",
            "type",
            "rpn",
            "data",
        ];

        fn new() -> Self {
            Self(0)
        }
        fn set_all(&mut self, enable: bool) {
            // Do not enable data when requesting "all"
            if enable {
                self.0 |= 0x7F;
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

    #[derive(Debug)]
    pub struct AssertionFeatures(u8);
    impl AssertionFeatures {
        pub const SRC: usize = 0;
        pub const OFFSET: usize = 1;
        pub const SECTION: usize = 2;
        pub const PCOFFSET: usize = 3;
        pub const TYPE: usize = 4;
        pub const RPN: usize = 5;
        pub const DATA: usize = 6;
        pub const MESSAGE: usize = 7;
    }
    impl Default for AssertionFeatures {
        fn default() -> Self {
            Self(1 << Self::SRC | 1 << Self::OFFSET | 1 << Self::MESSAGE)
        }
    }
    impl Features for AssertionFeatures {
        const KEYWORDS: &'static [&'static str] = &[
            "src", "offset", "section", "pcoffset", "type", "rpn", "data", "message",
        ];

        fn new() -> Self {
            Self(0)
        }
        fn set_all(&mut self, enable: bool) {
            // Do not enable data when requesting "all"
            if enable {
                self.0 |= 0xBF;
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

    pub fn parse_keyword_list<F: Features>(arg: &str) -> Result<F, String> {
        let mut features = F::new();

        for keyword in arg.split(',') {
            let keyword = keyword.trim();

            // An initial dash prefix negates the keyword
            let (keyword, enable) = if let Some('-') = keyword.chars().next() {
                (&keyword[1..], false)
            } else {
                (keyword, true)
            };

            if keyword.is_empty() {
                return Err("Empty keyword".into());
            }

            if keyword.eq_ignore_ascii_case("all") {
                features.set_all(enable);
            } else {
                // See which keyword this matches; if none, report the error
                let which = F::KEYWORDS
                    .iter()
                    .enumerate()
                    .find_map(|(i, candidate)| keyword.eq_ignore_ascii_case(candidate).then(|| i))
                    .ok_or_else(|| {
                        let mut err =
                            format!("Unknown keyword \"{}\", expected one of: all", keyword);
                        for keyword in F::KEYWORDS {
                            err.push_str(", ");
                            err.push_str(keyword);
                        }
                        err
                    })?;
                features.set(which, enable);
            }
        }

        Ok(features)
    }

    pub fn parse<F: Features>(matches: &ArgMatches, name: &str) -> F {
        matches.value_of(name).map_or_else(
            || {
                if matches.is_present(name) {
                    Default::default()
                } else {
                    F::new()
                }
            },
            |val| parse_keyword_list(val).unwrap(),
        )
    }
}
use features::*;

pub fn parse() -> Args {
    let matches = clap_app!(rgbobj =>
        (version: "v0.1.0")
        (about: "Prints out RGBDS object files in a human-friendly manner")
        (after_help: "A keyword list is a comma-separated list of keywords, with whitespace ignored around keywords.\nA keyword can be prefixed with a dash '-', negating its effect; note that whitespace is not permitted between the dash and the keyword, and that the first keyword may not be negated.\nKeyword lists can be omitted, in which case they default to the \"min\" value.\n\nSee `man 1 rgbobj` for more information, including what each keyword does.")
        (@arg all: -A --all "Display all output for all output types; this overrides any other display option")
        (@arg color: --color possible_value[auto always never] default_value[auto] +case_insensitive "Whether to color output")
        (@arg rpn: -r --rpn [format] possible_value[expr infix] default_value[expr] +case_insensitive "The format to use for printing RPN")
        (@arg header: -h --header [keywords] #{0,1} {parse_keyword_list::<HeaderFeatures>} "Keyword list of what to display about the header (min: counts)")
        (@arg symbol: -y --symbol [keywords] #{0,1} {parse_keyword_list::<SymbolFeatures>} "Keyword list of what to display about symbols (min: name)")
        (@arg section: -s --section [keywords] #{0,1} {parse_keyword_list::<SectionFeatures>} "Keyword list of what to display about sections (min: name)")
        (@arg patch: -p --patch [keywords] #{0,1} {parse_keyword_list::<PatchFeatures>} "Keyword list of what to display about patches (min: count)")
        (@arg assertion: -a --assertion [keywords] #{0,1} {parse_keyword_list::<AssertionFeatures>} "Keyword list of what to display about assertions (min: file,offset,message)")
        (@arg file: * +hidden)
    )
    .get_matches();

    let color_choice = matches.value_of("color").unwrap();
    let (color_out, color_err) = if color_choice.eq_ignore_ascii_case("always") {
        (ColorChoice::Always, ColorChoice::Always)
    } else if color_choice.eq_ignore_ascii_case("never") {
        (ColorChoice::Never, ColorChoice::Never)
    } else {
        assert!(
            color_choice.eq_ignore_ascii_case("auto"),
            "{} not \"auto\"!?",
            color_choice
        );

        use atty::Stream::*;
        let auto = |stream| {
            if atty::is(stream) {
                ColorChoice::Auto
            } else {
                ColorChoice::Never
            }
        };
        (auto(Stdout), auto(Stderr))
    };

    let mut header = features::parse::<HeaderFeatures>(&matches, "header");
    let mut symbol = features::parse::<SymbolFeatures>(&matches, "symbol");
    let mut section = features::parse::<SectionFeatures>(&matches, "section");
    let mut patch = features::parse::<PatchFeatures>(&matches, "patch");
    let mut assertion = features::parse::<AssertionFeatures>(&matches, "assertion");
    if matches.is_present("all") {
        header.set_all(true);
        symbol.set_all(true);
        section.set_all(true);
        patch.set_all(true);
        assertion.set_all(true);
    }

    Args {
        rpn: RpnPrintType::from_str(matches.value_of("rpn").unwrap()),

        header,
        symbol,
        section,
        patch,
        assertion,

        path: matches.value_of_os("file").unwrap().into(),

        color_out,
        color_err,
    }
}

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

#[derive(Debug)]
pub enum RpnPrintType {
    Expr,
    Infix,
}

impl RpnPrintType {
    fn from_str(string: &str) -> Self {
        let string = string.trim();
        if string.eq_ignore_ascii_case("expr") {
            Self::Expr
        } else {
            assert!(
                string.eq_ignore_ascii_case("infix"),
                "{} not \"infix\"!?",
                string
            );
            Self::Infix
        }
    }

    pub fn name(&self) -> &'static str {
        use RpnPrintType::*;

        match self {
            Expr => "RPN",
            Infix => "infix",
        }
    }
}
