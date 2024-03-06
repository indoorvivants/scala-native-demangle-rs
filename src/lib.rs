//! Demangle Scala Native identifiers
//!
//! Turn mangled Scala Native identifiers into a more readable form.
//!
//! 1. Name mangling rules: https://scala-native.org/en/latest/contrib/mangling.html
//! 2. Scala implementation: https://github.com/indoorvivants/sn-demangler

pub type DemangleError = String;
pub type ParsingResult = Result<String, DemangleError>;

pub struct DemanglingConfig {
    pub collapse_scala_names: bool,
    pub debug: bool,
}

impl Default for DemanglingConfig {
    fn default() -> Self {
        DemanglingConfig {
            collapse_scala_names: true,
            debug: false,
        }
    }
}

impl DemanglingConfig {
    fn log(&self, str: &str) -> () {
        if self.debug {
            println!("{str}")
        }
    }
    fn log_name(&self, name: &str, str: &str) -> () {
        if self.debug {
            println!("{name}: {str}")
        }
    }
}

pub fn demangle(input: &str, config: &DemanglingConfig) -> ParsingResult {
    if !input.starts_with("_S") {
        return Err("identifier doesn't start with _S".to_string());
    } else {
        config.log(format!("demangle: {}", &input[2..]).as_str());
        return defn_name(&input[2..], config);
    }
}

// private sub parsers
// <defn-name> ::=
//     T <name>                       // top-level name
//     M <name> <sig-name>            // member name
fn defn_name(input: &str, config: &DemanglingConfig) -> ParsingResult {
    config.log_name("defn_name", input);
    if input.starts_with("T") {
        return toplevel_name(&input[1..], config);
    } else if input.starts_with("M") {
        return member_name(&input[1..], config);
    } else {
        if input.len() > 0 {
            return Err(format!(
                "defn_name: unknown name modifier '{}'",
                input[0..0].to_string()
            ));
        } else {
            return Err("defn_name: unexpectedly empty rest of identifier".to_string());
        }
    }
}

fn toplevel_name(input: &str, config: &DemanglingConfig) -> ParsingResult {
    config.log_name("toplevel_name", input);
    return name(input, config).1;
}
fn member_name(input: &str, config: &DemanglingConfig) -> ParsingResult {
    config.log_name("member_name", input);
    let (consumed, owner) = name(input, config);
    let signature = sig_name(&input[consumed..], config);

    let result =
        owner.and_then(|ow| return signature.and_then(|s| return Ok(format!("{}.{}", ow, s))));
    return result;
}

// <sig-name> ::=
//     F <name> <scope>                    // field name
//     R <type-name>+ E                    // constructor name
//     D <name> <type-name>+ E <scope>     // method name
//     P <name> <type-name>+ E             // proxy name
//     C <name>                            // c extern name
//     G <name>                            // generated name
//     K <sig-name> <type-name>+ E         // duplicate name
fn sig_name(input: &str, config: &DemanglingConfig) -> ParsingResult {
    config.log_name("sig_name", input);
    if input.starts_with("C") || input.starts_with("G") {
        return name(&input[1..], config).1;
    } else if input.starts_with("F") {
        let (consumed, field_name) = name(&input[1..], config);
        return field_name.and_then(|nm| {
            let rest = &input[(1 + consumed)..];
            return scope(rest, config).map(|sc| format!("{}{}", render_scope(sc), nm));
        });
    } else if input.starts_with("R") {
        let type_names = read_type_names(&input[1..], config);
        return type_names.1.map(|vc| vc.join(", "));
    } else if input.starts_with("K") {
        // TODO: basically the same as D case below
        let after_tag = &input[1..];
        let (consumed, nm) = name(after_tag, config);

        let after_name = &after_tag[consumed..];
        let (_, type_names) = read_type_names(after_name, config);

        type_names.and_then(|tn| {
            nm.map(|nm| {
                let signature = match tn.len() {
                    1 => format!("{}: {}", nm, tn.join(",")),
                    n => format!(
                        "{}({}): {}",
                        nm,
                        tn[0..n - 2].join(","),
                        tn.get(n - 1).unwrap()
                    ),
                };

                return signature;
            })
        })
    } else if input.starts_with("P") {
        // TODO: basically the same as D case below
        let after_tag = &input[1..];
        let (consumed, nm) = name(after_tag, config);

        let after_name = &after_tag[consumed..];
        let (_, type_names) = read_type_names(after_name, config);

        type_names.and_then(|tn| {
            nm.map(|nm| {
                let signature = match tn.len() {
                    1 => format!("{}: {}", nm, tn.join(",")),
                    n => format!(
                        "{}({}): {}",
                        nm,
                        tn[0..n - 2].join(","),
                        tn.get(n - 1).unwrap()
                    ),
                };

                return signature;
            })
        })
    } else if input.starts_with("D") {
        let after_tag = &input[1..];
        let (consumed, nm) = name(after_tag, config);

        let after_name = &after_tag[consumed..];
        let (consumed, type_names) = read_type_names(after_name, config);

        let after_types = &after_name[consumed + 1..];
        config.log_name(
            "sig_name:D",
            format!("type_names: {type_names:?}, after: {after_types}").as_str(),
        );
        let sc = scope(&after_types, config);

        type_names.and_then(|tn| {
            nm.and_then(|nm| {
                sc.map(|sc| {
                    let signature = match tn.len() {
                        1 => format!("{}{}: {}", render_scope(sc), nm, tn.join(",")),
                        n => format!(
                            "{}{}({}): {}",
                            render_scope(sc),
                            nm,
                            tn[0..n - 1].join(","),
                            tn.get(n - 1).unwrap()
                        ),
                    };

                    return signature;
                })
            })
        })
    } else {
        return Err(
            format!("sig_name: expected to start with F/R/D/P/C/G/K, {}", &input).to_string(),
        );
    }
}

fn read_type_names(input: &str, config: &DemanglingConfig) -> (usize, Result<Vec<String>, String>) {
    let mut pos = 0;
    let mut result = Vec::new();
    while !input[pos..].starts_with("E") {
        let (consumed, nm) = type_name(&input[pos..], config);
        result.push(nm.unwrap());
        pos += consumed;
    }

    return (pos, Ok(result));
}

fn scala_root_name(name: &str, config: &DemanglingConfig) -> String {
    if !config.collapse_scala_names {
        return format!("scala.{name}");
    } else {
        return name.to_string();
    };
}

fn common_type_name(name: String, config: &DemanglingConfig) -> String {
    if !config.collapse_scala_names {
        return name;
    } else {
        let immut = "scala.collection.immutable.";

        if name == "java.lang.Object" {
            return "Object".to_string();
        } else if name == "java.lang.String" {
            return "String".to_string();
        } else if name == "java.lang.Throwable" {
            return "Throwable".to_string();
        } else if name.starts_with(immut) {
            return name.strip_prefix(immut).unwrap_or(&name).to_string();
            // return "Throwable".to_string();
        } else {
            return name;
        }
    }
}

// <type-name> ::=
//     v                              // c vararg
//     R _                            // c pointer type-name
//     R <type-name>+ E               // c function type-name
//     S <type-name>+ E               // c anonymous struct type-name
//     A <type-name> <number> _       // c array type-name
//     <integer-type-name>            // signed integer type-name
//     <integer-type-name> ::=
//         b                              // scala.Byte
//         s                              // scala.Short
//         i                              // scala.Int
//         j                              // scala.Long
//     z                              // scala.Boolean
//     c                              // scala.Char
//     f                              // scala.Float
//     d                              // scala.Double
//     u                              // scala.Unit
//     l                              // scala.Null
//     n                              // scala.Nothing
//     L <nullable-type-name>         // nullable type-name
//     A <type-name> _                // nonnull array type-name
//     X <name>                       // nonnull exact class type-name
//     <name>                         // nonnull class type-name
fn type_name(input: &str, config: &DemanglingConfig) -> (usize, ParsingResult) {
    let mut chars = input.chars();
    config.log(format!("type_name: {input}").as_str());

    let scala_root_namer = |name: &str| scala_root_name(name, config);
    let common_type_namer = |name: String| common_type_name(name, config);

    let result = match chars.next() {
        Some('v') => (1, Ok("<c vararg>".to_string())),
        Some('z') => (1, Ok(scala_root_namer("Boolean"))),
        Some('c') => (1, Ok(scala_root_namer("Char"))),
        Some('f') => (1, Ok(scala_root_namer("Float"))),
        Some('d') => (1, Ok(scala_root_namer("Double"))),
        Some('u') => (1, Ok(scala_root_namer("Unit"))),
        Some('l') => (1, Ok(scala_root_namer("Null"))),
        Some('n') => (1, Ok(scala_root_namer("Nothing"))),
        Some('b') => (1, Ok(scala_root_namer("Byte"))),
        Some('s') => (1, Ok(scala_root_namer("Short"))),
        Some('i') => (1, Ok(scala_root_namer("Int"))),
        Some('j') => (1, Ok(scala_root_namer("Long"))),

        Some('R') => match chars.next() {
            Some('_') => (2, Ok("<c pointer>".to_string())),
            Some(c) => (
                0,
                Err(format!("type_name: after R expected _, got `{c}` instead").to_string()),
            ),
            None => (0, Err("type_name: unexpected end of input".to_string())),
        },
        Some('L') => {
            let (consumed, type_name) = nullable_type_name(&input[1..], config);
            (consumed + 1, type_name.map(common_type_namer))
        }
        Some('A') => {
            let (consumed, tn) = type_name(&input[1..], config);
            let after_type_name = &input[1 + consumed..];
            let num = number(after_type_name);
            (
                consumed + num + 1, /* "_" at the end */
                tn.map(|t| format!("CArray[{}]", t)),
            )
        }
        Some('X') => {
            let (consumed, class_type_name) = name(&input[1..], config);
            (consumed + 1, class_type_name)
        }
        Some(other) => (
            0,
            Err(format!("type_name: unexpected start character `{other}`").to_string()),
        ),
        None => (0, Err("type_name: unexpected end of input".to_string())),
    };

    return result;
}

fn number(input: &str) -> usize {
    return input.chars().take_while(|c| c.is_digit(10)).count();
}

fn nullable_type_name(input: &str, config: &DemanglingConfig) -> (usize, ParsingResult) {
    let mut chars = input.chars();

    match chars.next() {
        Some('A') => {
            let (consumed, n) = type_name(&input[1..], config);
            return (consumed + 2, n.map(|ar| format!("Array[{}]", ar)));
        }
        Some('X') => {
            let (consumed, n) = name(input, config);

            return (consumed + 1, n);
        }
        Some(d) if d.is_digit(10) => {
            return name(input, config);
        }
        Some(a) => {
            return (
                0,
                Err(format!("nullable_type_name: unexpected start `{a}`")),
            )
        }
        None => {
            return (
                0,
                Err("nullable_type_name: unexpected end of input".to_string()),
            )
        }
    };
}

enum Scope {
    Public,
    PublicStatic,
    Private(String),
}

fn render_scope(scope: Scope) -> String {
    return match scope {
        Scope::Public => "".to_string(),
        Scope::PublicStatic => "".to_string(),
        Scope::Private(inn) => format!("<private[{}]>", inn),
    };
}

// <scope> ::=
//     P <defn-name>                  // private to defn-name
//     O                              // public
fn scope(input: &str, config: &DemanglingConfig) -> Result<Scope, String> {
    config.log_name("scope", input);
    if input.starts_with("O") {
        return Ok(Scope::Public);
    } else if input.starts_with("o") {
        return Ok(Scope::PublicStatic);
    } else if input.starts_with("P") {
        return defn_name(&input[1..], config).map(|i| return Scope::Private(i));
    } else {
        return Err(format!("scope: cannot read `{}`", input).to_string());
    }
}

fn name(input: &str, config: &DemanglingConfig) -> (usize, ParsingResult) {
    //println!("name: {}", input);
    config.log_name("name", input);
    let mut number_end: usize = 0;
    for c in input.chars() {
        if c.is_digit(10) {
            number_end += 1
        } else {
            break;
        }
    }
    if number_end >= 1 {
        let (length, rest) = input.split_at(number_end);

        match usize::from_str_radix(length, 10) {
            Ok(res) => {
                if rest.starts_with("-") {
                    return (length.len() + 1 + res, Ok(rest[1..(1 + res)].to_string()));
                } else {
                    return (length.len() + res, Ok(rest[0..res].to_string()));
                }
            }
            Err(_) => {
                return (0, Err("name: invalid length".to_string()));
            }
        }
    } else {
        return (
            0,
            Err(format!("name: invalid input `{}`", input.to_string())),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::demangle;

    fn run(s: &str) -> String {
        return demangle(s, &Default::default()).unwrap();
    }

    fn run_raw(s: &str) -> String {
        return demangle(
            s,
            &crate::DemanglingConfig {
                collapse_scala_names: false,
                ..Default::default()
            },
        )
        .unwrap();
    }

    #[test]
    fn it_works() {
        assert_eq!(run("_ST10__dispatch"), "__dispatch");
        assert_eq!(
            run_raw("_SM42sttp.model.headers.CacheDirective$MinFreshD12productArityiEO"),
            "sttp.model.headers.CacheDirective$MinFresh.productArity: scala.Int"
        );
        assert_eq!(
            run("_SM42sttp.model.headers.CacheDirective$MinFreshD12productArityiEO"),
            "sttp.model.headers.CacheDirective$MinFresh.productArity: Int"
        );

        assert_eq!(run_raw("_SM42scala.scalanative.runtime.SymbolFormatter$D10inBounds$1L32scala.scalanative.unsigned.ULongizEPT42scala.scalanative.runtime.SymbolFormatter$"), 
            "scala.scalanative.runtime.SymbolFormatter$.<private[scala.scalanative.runtime.SymbolFormatter$]>inBounds$1(scala.scalanative.unsigned.ULong,scala.Int): scala.Boolean");

        assert_eq!(run("_SM42scala.scalanative.runtime.SymbolFormatter$D10inBounds$1L32scala.scalanative.unsigned.ULongizEPT42scala.scalanative.runtime.SymbolFormatter$"), 
            "scala.scalanative.runtime.SymbolFormatter$.<private[scala.scalanative.runtime.SymbolFormatter$]>inBounds$1(scala.scalanative.unsigned.ULong,Int): Boolean");

        assert_eq!(run_raw("_SM41scalaboot.template.scalatemplate$package$D10$anonfun$3L26scalaboot.template.ContextL15scala.Function1L23java.lang.StringBuilderL31scalaboot.template.UnsafeCursorL23scalaboot.template.MoveuEPT41scalaboot.template.scalatemplate$package$"), 
            "scalaboot.template.scalatemplate$package$.<private[scalaboot.template.scalatemplate$package$]>$anonfun$3(scalaboot.template.Context,scala.Function1,java.lang.StringBuilder,scalaboot.template.UnsafeCursor,scalaboot.template.Move): scala.Unit");

        assert_eq!(run("_SM41scalaboot.template.scalatemplate$package$D10$anonfun$3L26scalaboot.template.ContextL15scala.Function1L23java.lang.StringBuilderL31scalaboot.template.UnsafeCursorL23scalaboot.template.MoveuEPT41scalaboot.template.scalatemplate$package$"), 
            "scalaboot.template.scalatemplate$package$.<private[scalaboot.template.scalatemplate$package$]>$anonfun$3(scalaboot.template.Context,scala.Function1,java.lang.StringBuilder,scalaboot.template.UnsafeCursor,scalaboot.template.Move): Unit");

        assert_eq!(run("_SM33scala.scalanative.unsafe.package$D11fromCStringL28scala.scalanative.unsafe.PtrL24java.nio.charset.CharsetL16java.lang.StringEO"), "scala.scalanative.unsafe.package$.fromCString(scala.scalanative.unsafe.Ptr,java.nio.charset.Charset): String");

        assert_eq!(run("_SM17java.lang.IntegerD7compareiiiEo"), "java.lang.Integer.compare(Int,Int): Int")
    }
}
