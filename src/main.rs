use rgbds_obj::{Node, NodeType, NodeWalkError, Object};
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::{self, BufReader, Write};
use std::path::Path;
use std::process;
use termcolor::{ColorChoice, ColorSpec, StandardStream, WriteColor};

mod args;
use args::features::*;
use args::{Args, RpnPrintType};

fn main() {
    // Don't panic when printing to a closed pipe, e.g. with `rgbobj | head`.
    sigpipe::reset();

    let args = args::parse();

    if let Err(err) = work(&args) {
        let mut stderr = StandardStream::stderr(args.color_err);
        stderr
            .set_color(
                ColorSpec::new()
                    .set_fg(Some(termcolor::Color::Red))
                    .set_bold(true),
            )
            .unwrap();
        write!(stderr, "error: ").unwrap();
        stderr.reset().unwrap();
        writeln!(stderr, "{err}").unwrap();

        process::exit(1);
    }
}

macro_rules! plural {
    ($n:expr, $many:expr, $one:expr) => {
        if ($n) == 1 {
            $one
        } else {
            $many
        }
    };
    ($n:expr, $many:expr) => {
        plural!($n, $many, "")
    };
}

fn work(args: &Args) -> Result<(), MainError> {
    macro_rules! error {
        ($($args:tt)+) => {
            let mut stderr = StandardStream::stderr(args.color_err);
            stderr
                .set_color(
                    ColorSpec::new()
                        .set_bold(true)
                        .set_fg(Some(termcolor::Color::Red)),
                )
                .unwrap();
            write!(stderr, "error: ").unwrap();
            stderr
                .set_color(ColorSpec::new().set_bold(true).set_fg(None))
                .unwrap();
            writeln!(stderr, $($args)+)
                .unwrap();
            stderr.reset().unwrap();
        };
    }
    macro_rules! warning {
        ($($args:tt)+) => {
            let mut stderr = StandardStream::stderr(args.color_err);
            stderr
                .set_color(
                    ColorSpec::new()
                        .set_bold(true)
                        .set_fg(Some(termcolor::Color::Yellow)),
                )
                .unwrap();
            write!(stderr, "warning: ").unwrap();
            stderr
                .set_color(ColorSpec::new().set_bold(true).set_fg(None))
                .unwrap();
            writeln!(stderr, $($args)+)
                .unwrap();
            stderr.reset().unwrap();
        };
    }

    let mut file = BufReader::new(File::open(&args.path)?);
    let object = Object::read_from(&mut file)?;
    let mut stdout = StandardStream::stdout(args.color_out);

    macro_rules! print {
        ($($args:tt)*) => { write!(stdout, $($args)*).unwrap() };
    }
    macro_rules! println {
        ($($args:tt)*) => { writeln!(stdout, $($args)*).unwrap() };
    }
    macro_rules! setup {
        ($($args:tt)+) => { stdout.set_color(color_spec!($($args)+)).unwrap() };
    }
    macro_rules! color_spec {
        () => { ColorSpec::new() };
        (bold $($tail:tt)*) => { color_spec!($($tail)*).set_bold(true) }
    }
    macro_rules! reset {
        () => {
            stdout.reset().unwrap()
        };
    }

    macro_rules! print_header {
        ($str:literal) => {
            println!();
            println!();
            setup!(bold);
            println!($str);
            println!("{}", "-".repeat($str.len()));
            reset!();
            println!();
        };
    }

    macro_rules! print_source_nodes {
        ($source:expr) => {{
            let (id, line_no) = ($source).source();
            // TODO: wrap the node stack nicely
            object
                .walk_nodes::<Infallible, _>(id, &mut |node: &Node| {
                    if let Some((id, line)) = node.parent() {
                        print!("({line}) -> ");
                        // REPT nodes are prefixed by their parent's name
                        if matches!(node.type_data(), NodeType::Rept(..)) {
                            print!(
                                "{}",
                                object
                                    .node(id)
                                    .ok_or_else(|| NodeWalkError::bad_id(id, &object))?
                                    .type_data()
                            );
                        }
                    } else {
                        // The root node should not be a REPT one
                        if matches!(node.type_data(), NodeType::Rept(..)) {
                            print!("<error>");
                            error!("REPT-type root file stack node");
                        }
                    }
                    print!("{}", node.type_data());
                    Ok(())
                })
                .and_then(|()| -> Result<_, NodeWalkError<Infallible>> {
                    print!("({line_no})");
                    Ok(())
                })
        }};
    }

    macro_rules! print_rpn_expr {
        ($indent:expr, $expr:expr) => {
            println!("{}{} expression:", $indent, args.rpn);
            print!("{}    ", $indent);
            // TODO: wrap the RPN expression nicely
            if matches!(args.rpn, RpnPrintType::Infix) {
                println!("{:#}", $expr);
            } else {
                println!("{}", $expr);
            }
        };
    }

    macro_rules! print_rpn_data {
        ($indent:expr, $data:expr) => {
            let len = ($data).bytes().len();
            println!("{}RPN data ({len} byte{}):", $indent, plural!(len, "s"));

            print!("{}    [{:02x}", $indent, ($data).bytes()[0]);
            let mut i = 1;
            for byte in &($data).bytes()[1..] {
                if i % 16 == 0 {
                    println!();
                    print!("{}     ", $indent);
                } else {
                    print!(" ");
                }
                print!("{byte:02x}");
                i += 1;
            }
            println!("]");
        };
    }

    // Print header

    setup!(bold);
    print!(
        "{}",
        Path::new(&args.path).file_name().unwrap().to_string_lossy()
    );
    reset!();
    if args.header.get(HeaderFeatures::SIZE) {
        let len = file.get_ref().metadata().unwrap().len();
        print!(" [{len} byte{}]", plural!(len, "s"));
    }
    println!(
        ": RGBDS object v{} revision {}",
        object.version(),
        object.revision()
    );

    if args.header.get(HeaderFeatures::COUNTS) {
        println!("    Symbols: {}", object.symbols().len());
        println!("    Sections: {}", object.sections().len());
        println!("    Assertions: {}", object.assertions().len());
    }

    // Print symbols

    if args.symbol.any() && !object.symbols().is_empty() {
        print_header!("Symbols");

        let mut separate_lines = false;

        for (symbol_id, symbol) in object.symbols().iter().enumerate() {
            if separate_lines {
                println!();
            }

            let mut printed_lines = 0;
            let mut first_line_empty = true;

            if args.symbol.get(SymbolFeatures::NAME) {
                print!("{}", String::from_utf8_lossy(symbol.name()));
                first_line_empty = false;
            }

            if args.symbol.get(SymbolFeatures::ID) {
                if !first_line_empty {
                    print!(" ");
                }
                print!("(#{})", symbol_id);
                first_line_empty = false;
            }

            if args.symbol.get(SymbolFeatures::TYPE) {
                if !first_line_empty {
                    print!(" ");
                }
                print!("[{}]", symbol.visibility().name());
                first_line_empty = false;
            }

            if !first_line_empty {
                println!();
                printed_lines += 1;
            }

            let indent = if printed_lines == 0 { "" } else { "    " };

            if let Some(data) = symbol.visibility().data() {
                if args.symbol.get(SymbolFeatures::SRC) {
                    print!("{indent}");
                    if let Err(err) = print_source_nodes!(data) {
                        error!("Invalid symbol file stack: {}", err);
                    }
                    println!();
                    printed_lines += 1;
                }

                if args.symbol.get(SymbolFeatures::SECTION) {
                    print!("{indent}");
                    if let Some(id) = data.section() {
                        print!(
                            "SECTION: \"{}\"",
                            String::from_utf8_lossy(
                                object.sections()[usize::try_from(id).unwrap()].name()
                            )
                        );
                    } else {
                        print!("Constant");
                    }
                    if args.symbol.get(SymbolFeatures::VALUE) {
                        print!(" "); // Pad between the section name and value
                    } else {
                        println!();
                        printed_lines += 1;
                    }
                }
                if args.symbol.get(SymbolFeatures::VALUE) {
                    println!("[@ ${:04x}]", data.value());
                    printed_lines += 1;
                }
            }

            if printed_lines > 1 {
                separate_lines = true;
            }
        }
    }

    // Print sections

    if args.section.any() && !object.sections().is_empty() {
        print_header!("Sections");

        let mut separate_lines = false;

        for section in object.sections().iter() {
            if separate_lines {
                println!();
            }

            let mut printed_lines = 0;
            let mut first_line_empty = true;

            if args.section.get(SectionFeatures::NAME) {
                print!("SECTION");
                if args.section.get(SectionFeatures::TYPE) {
                    if let Some(modifier) = section.modifier().name() {
                        print!(" {modifier}");
                    }
                }
                print!(" \"{}\"", String::from_utf8_lossy(section.name()));
                first_line_empty = false;
            }

            if args.section.get(SectionFeatures::TYPE) {
                if !first_line_empty {
                    print!(", ");
                }
                // If the name has been printed, then the modifier already has been
                if !args.section.get(SectionFeatures::NAME) {
                    if let Some(modifier) = section.modifier().name() {
                        print!("{modifier} ");
                    }
                }
                print!("{}", section.type_data().name());
                first_line_empty = false;
            }

            if args.section.get(SectionFeatures::ORG) {
                if let Some(org) = section.org() {
                    if !first_line_empty && !args.section.get(SectionFeatures::TYPE) {
                        print!(" ");
                    }
                    print!("[${org:04x}]");
                } else if first_line_empty {
                    print!("Floating");
                }
                first_line_empty = false;
            }

            if args.section.get(SectionFeatures::BANK) && section.type_data().is_banked() {
                if let Some(bank) = section.bank() {
                    if !first_line_empty {
                        print!(", ");
                    }
                    print!("BANK[${bank:04x}]");
                } else if first_line_empty {
                    print!("Floating");
                }
                first_line_empty = false;
            }

            if args.section.get(SectionFeatures::ALIGN) {
                let align = section.align();
                let ofs = section.align_ofs();

                if align > 16 {
                    warning!("Invalid alignment ({align}) greater than 16");
                    // Skip checking for the other warning to avoid over-shifting
                } else if ofs >= 1 << align {
                    warning!(
                        "Invalid alignment offset ({ofs}) greater than alignment size ({})",
                        1 << align
                    );
                }
                if align != 0 {
                    if !first_line_empty {
                        print!(", ");
                    }
                    if ofs == 0 {
                        print!("ALIGN[{align}]");
                    } else {
                        print!("ALIGN[{align}, {ofs}]");
                    }
                    first_line_empty = false;
                }
            }

            if !first_line_empty {
                println!();
                printed_lines += 1;
            }

            let indent = if first_line_empty { "" } else { "    " };

            if args.section.get(SectionFeatures::SIZE) {
                let len = section.size();
                println!(
                    "{indent}${len:04x} ({len}) byte{}{}",
                    plural!(len, "s"),
                    if len > 0 && args.section.get(SectionFeatures::DATA) {
                        ":"
                    } else {
                        ""
                    }
                );
                printed_lines += 1;
            }

            if let Some(data) = section.type_data().data() {
                if args.section.get(SectionFeatures::DATA) {
                    let mut patched_ranges = Vec::with_capacity(data.patches().len());
                    // Don't bother if we're not gonna color the output
                    if args.color_out != ColorChoice::Never {
                        for patch in data.patches() {
                            let (ofs, size) = (patch.offset(), patch.patch_type().size());
                            let index = patched_ranges.partition_point(|&(val, _)| val <= ofs);
                            patched_ranges.insert(index, (ofs, size));
                        }
                    }

                    let mut patched_ranges = patched_ranges.iter().peekable();
                    let mut patch_color_spec = ColorSpec::new();
                    patch_color_spec.set_bg(Some(termcolor::Color::Magenta));
                    let mut i = 0;

                    for byte in data.data() {
                        // At the start of a new line, print indent (if any) and current offset
                        if i % 16 == 0 {
                            print!("{indent}{i:04x}:");
                            // If a line break occurred in the middle of a patched range, reinstate it
                            if patched_ranges.peek().map_or(false, |(ofs, _)| i > *ofs) {
                                stdout.set_color(&patch_color_spec).unwrap();
                            }
                        }
                        // Group bytes two by two (OK at the start of a line, after the colon)
                        if i % 2 == 0 {
                            print!(" ");
                        }
                        // If about to enter a patched region, use a different BG color
                        if patched_ranges.peek().map_or(false, |(ofs, _)| i == *ofs) {
                            stdout.set_color(&patch_color_spec).unwrap();
                        }
                        print!("{byte:02x}");
                        i += 1;
                        // If done with a patched range, go to the next one...
                        if patched_ranges
                            .peek()
                            .map_or(false, |(ofs, size)| i == *ofs + u32::from(*size))
                        {
                            // Now, normally we'd revert to normal output, *but* patches may
                            // overlap (this is invalid, but may happen nonetheless, and we should
                            // handle it gracefully). If that's the case, we're still within a
                            // patch, so don't revert.

                            // Check if the next patch is already behind us
                            // (This implies some overlap, which is invalid, so warn about it)
                            loop {
                                patched_ranges.next(); // Advance to the next range
                                match patched_ranges.peek() {
                                    // If the range ended before the current position, ignore it
                                    // and try again
                                    Some((ofs, size)) if *ofs + u32::from(*size) <= i => {
                                        continue;
                                    }
                                    // If we're already in that range, keep the BG enabled
                                    Some((ofs, _)) if *ofs < i => {
                                        break;
                                    }
                                    // Otherwise, reset the background
                                    _ => {
                                        stdout.reset().unwrap();
                                        break;
                                    }
                                }
                            }
                        }
                        // If at the end of a line, print an EOL
                        if i % 16 == 0 {
                            stdout.reset().unwrap(); // Make sure not to print the "metadata" in patched form
                            println!();
                            printed_lines += 1;
                        }
                    }
                    // Make sure to end on a newline, and clear any patch leftovers
                    if i % 16 != 0 {
                        stdout.reset().unwrap();
                        println!();
                        printed_lines += 1;
                    }
                }

                if args.section.get(SectionFeatures::DATA)
                    && !data.data().is_empty()
                    && args.patch.any()
                {
                    println!(); // Pad between the section data and patches
                    printed_lines += 1;
                }

                if args.patch.any() {
                    if args.patch.get(PatchFeatures::COUNT) {
                        let len = data.patches().len();
                        println!(
                            "{indent}{len} patch{}{}",
                            plural!(len, "es"),
                            if len > 0 && args.patch.any_besides(PatchFeatures::COUNT) {
                                ":"
                            } else {
                                ""
                            }
                        );
                        printed_lines += 1;
                    }

                    for patch in data.patches() {
                        let mut patch_line_empty = true;

                        if args.patch.get(PatchFeatures::SRC) {
                            print!("{indent}");
                            if let Err(err) = print_source_nodes!(patch) {
                                error!("Invalid patch file stack: {err}");
                            }
                            patch_line_empty = false;
                        }

                        if args.patch.get(PatchFeatures::OFFSET) {
                            print!(
                                "{}@ ${:04x}",
                                if patch_line_empty { indent } else { " " },
                                patch.offset()
                            );
                            patch_line_empty = false;
                        }

                        if args.patch.get(PatchFeatures::TYPE) {
                            print!(
                                "{}({})",
                                if patch_line_empty { indent } else { " " },
                                patch.patch_type().name()
                            );
                            patch_line_empty = false;
                        }

                        if !patch_line_empty {
                            println!();
                            printed_lines += 1;
                        }

                        let patch_indent = if patch_line_empty { "" } else { "    " };

                        if args.patch.get(PatchFeatures::PCSECTION)
                            || args.patch.get(PatchFeatures::PCOFFSET)
                        {
                            print!("{indent}{patch_indent}PC");
                            if args.patch.get(PatchFeatures::PCSECTION) {
                                let pc_section_id = patch.pc_section_id();

                                if let Some(section) = object
                                    .sections()
                                    .get(usize::try_from(pc_section_id).unwrap())
                                {
                                    print!(" in \"{}\"", String::from_utf8_lossy(section.name()));
                                } else {
                                    error!(
                                        "Patch has PC section #{pc_section_id} out of {}",
                                        object.sections().len()
                                    );
                                    print!(" in <error>");
                                }
                            }
                            if args.patch.get(PatchFeatures::PCOFFSET) {
                                print!(" @ ${:04x}", patch.pc_offset());
                            }
                            println!();
                            printed_lines += 1;
                        }

                        let expr = patch.expr();
                        if expr.bytes().is_empty() {
                            error!("Empty patch RPN expression");
                        } else {
                            if args.patch.get(PatchFeatures::RPN) {
                                print_rpn_expr!(format!("{indent}{patch_indent}"), expr);
                                printed_lines += 2;
                            }
                            if args.patch.get(PatchFeatures::DATA) {
                                print_rpn_data!(format!("{indent}{patch_indent}"), expr);
                                printed_lines += 2;
                            }
                        }
                    }
                }
            }

            if printed_lines > 1 {
                separate_lines = true;
            }
        }
    }

    // Print assertions

    if args.assertion.any() && !object.assertions().is_empty() {
        print_header!("Assertions");

        let mut separate_lines = false;

        for assertion in object.assertions() {
            if separate_lines {
                println!();
            }

            let mut printed_lines = 0;
            let mut first_line_empty = true;

            if args.assertion.get(AssertionFeatures::SRC) {
                if let Err(err) = print_source_nodes!(assertion) {
                    error!("Invalid assertion file stack: {err}");
                }
                first_line_empty = false;
            }

            if args.assertion.get(AssertionFeatures::OFFSET) {
                print!(
                    "{}@ ${:04x}",
                    if first_line_empty { "" } else { " " },
                    assertion.offset()
                );
                first_line_empty = false;
            }

            if args.assertion.get(AssertionFeatures::TYPE) {
                print!(
                    "{}({})",
                    if first_line_empty { "" } else { " " },
                    assertion.err_type().name()
                );
                first_line_empty = false;
            }

            if !first_line_empty {
                println!();
                printed_lines += 1;
            }

            let indent = if first_line_empty { "" } else { "    " };

            if args.assertion.get(AssertionFeatures::SECTION)
                || args.assertion.get(AssertionFeatures::PCOFFSET)
            {
                print!("{indent}PC");
                if args.assertion.get(AssertionFeatures::SECTION) {
                    let pc_section_id = assertion.pc_section_id();

                    if let Some(section) = object
                        .sections()
                        .get(usize::try_from(pc_section_id).unwrap())
                    {
                        print!(" in \"{}\"", String::from_utf8_lossy(section.name()));
                    } else {
                        error!(
                            "Assertion has PC section #{pc_section_id} out of {}",
                            object.sections().len()
                        );
                        print!(" in <error>");
                    }
                }
                if args.assertion.get(AssertionFeatures::PCOFFSET) {
                    print!(" @ ${:04x}", assertion.pc_offset());
                }
                println!();
                printed_lines += 1;
            }

            let expr = assertion.expr();
            if expr.bytes().is_empty() {
                error!("Empty assertion RPN expression");
            } else {
                if args.assertion.get(AssertionFeatures::RPN) {
                    print_rpn_expr!(format!("{indent}"), expr);
                    printed_lines += 2;
                }
                if args.assertion.get(AssertionFeatures::DATA) {
                    print_rpn_data!(format!("{indent}"), expr);
                    printed_lines += 2;
                }
            }

            if args.assertion.get(AssertionFeatures::MESSAGE) {
                let msg = assertion.message();
                if msg.is_empty() {
                    println!("{indent}No message");
                } else {
                    println!("{indent}Message: \"{}\"", String::from_utf8_lossy(msg));
                }
                printed_lines += 1;
            }

            if printed_lines > 1 {
                separate_lines = true;
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
enum MainError {
    Io(io::Error),
}

impl Display for MainError {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmt::Error> {
        use MainError::*;

        match self {
            Io(err) => err.fmt(fmt),
        }
    }
}

impl Error for MainError {}

impl From<io::Error> for MainError {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}
