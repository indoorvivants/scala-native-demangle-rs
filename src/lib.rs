pub type ParsingResult = Result<String, String>;

pub fn scala_native_demangle(input: &str) -> ParsingResult {
    if !input.starts_with("_S") {
        return Err("identifier doesn't start with _S".to_string());
    } else {
        //println!("demangle: {}", &input[2..]);
        return defn_name(&input[2..]);
    }
}

fn defn_name(input: &str) -> ParsingResult {
    //println!("defn_name: {}", input);
    if input.starts_with("T") {
        return toplevel_name(&input[1..]);
    } else if input.starts_with("M") {
        return member_name(&input[1..]);
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

fn toplevel_name(input: &str) -> ParsingResult {
    //println!("toplevel_name: {}", input);
    return name(input).1;
}
fn member_name(input: &str) -> ParsingResult {
    //println!("member_name: {}", input);
    let (consumed, owner) = name(input);
    let signature = sig_name(&input[consumed..]);

    let result =
        owner.and_then(|ow| return signature.and_then(|s| return Ok(format!("{}.{}", ow, s))));
    return result;
}

fn sig_name(input: &str) -> ParsingResult {
    //println!("sig_name: {}", input);
    if input.starts_with("C") || input.starts_with("G") {
        return name(&input[1..]).1;
    } else if input.starts_with("F") {
        let (consumed, field_name) = name(&input[1..]);
        return field_name.and_then(|nm| {
            let rest = &input[(1 + consumed)..];
            return scope(rest).map(|sc| format!("{}{}", render_scope(sc), nm));
        });
    } else if input.starts_with("R") {
        let type_names = read_type_names(&input[1..]);
        return type_names.1.map(|vc| vc.join(", "));
    } else if input.starts_with("D") {
        let after_tag = &input[1..];
        let (consumed, nm) = name(after_tag);

        let after_name = &after_tag[consumed..];
        let (consumed, type_names) = read_type_names(after_name);

        let after_types = &after_name[consumed + 1..];
        let sc = scope(&after_types);

        type_names.and_then(|tn| {
            nm.and_then(|nm| {
                sc.map(|sc| {
                    let signature = match tn.len() {
                        1 => format!("{}{}: {}", render_scope(sc), nm, tn.join(",")),
                        n => format!(
                            "{}{}({}): {}",
                            render_scope(sc),
                            nm,
                            tn[0..n - 2].join(","),
                            tn.get(n - 1).unwrap()
                        ),
                    };

                    return signature;
                })
            })
        })

        // return type_names.map(|vc| format!("{}({})", nm.unwrap(), vc.join(",")));
    } else {
        return Err(
            format!("sig_name: expected to start with F/R/D/P/C/G/K, {}", &input).to_string(),
        );
    }
}

fn read_type_names(input: &str) -> (usize, Result<Vec<String>, String>) {
    let mut pos = 0;
    let mut result = Vec::new();
    while !input[pos..].starts_with("E") {
        let (consumed, nm) = type_name(&input[pos..]);
        result.push(nm.unwrap());
        pos += consumed;
    }

    return (pos, Ok(result));
}

fn type_name(input: &str) -> (usize, ParsingResult) {
    let mut chars = input.chars();
    //println!("type_name: {}", input);

    let result = match chars.next() {
        Some('v') => (1, Ok("<c vararg>".to_string())),
        Some('z') => (1, Ok("scala.Boolean".to_string())),
        Some('c') => (1, Ok("scala.Char".to_string())),
        Some('f') => (1, Ok("scala.Float".to_string())),
        Some('d') => (1, Ok("scala.Double".to_string())),
        Some('u') => (1, Ok("scala.Unit".to_string())),
        Some('l') => (1, Ok("scala.Null".to_string())),
        Some('n') => (1, Ok("scala.Nothing".to_string())),
        // integer-type-name inlined
        Some('b') => (1, Ok("scala.Byte".to_string())),
        Some('s') => (1, Ok("scala.Short".to_string())),
        Some('i') => (1, Ok("scala.Int".to_string())),
        Some('j') => (1, Ok("scala.Long".to_string())),

        Some('R') => match chars.next() {
            Some('_') => (2, Ok("<c pointer>".to_string())),
            other => todo!("type_name, R: {:?}", other),
        },
        Some('L') => {
            let (consumed, type_name) = nullable_type_name(&input[1..]);
            (consumed + 1, type_name)
        }
        Some('A') => {
            let (consumed, tn) = type_name(&input[1..]);
            let after_type_name = &input[1 + consumed..];
            let num = number(after_type_name);
            //println!("number: {}", num);
            (
                consumed + num + 1, /* "_" at the end */
                tn.map(|t| format!("CArray[{}]", t)),
            )
        }
        Some(other) => todo!("type_name: {:?}", other),
        None => todo!("handle missing next char"),
    };

    return result;
}

fn number(input: &str) -> usize {
    return input.chars().take_while(|c| c.is_digit(10)).count();
}

fn nullable_type_name(input: &str) -> (usize, ParsingResult) {
    //println!("nullable_type_name: {}", input);
    let mut chars = input.chars();

    match chars.next() {
        Some('A') => {
            let (consumed, n) = type_name(&input[1..]);

            return (consumed + 2, n.map(|ar| format!("Array[{}]", ar)));
        }
        Some('X') => {
            let (consumed, n) = name(input);

            return (consumed + 1, n);
        }
        Some(d) if d.is_digit(10) => {
            return name(input);
        }
        other => panic!("noooo {:?}", other),
    };
}

enum Scope {
    Public,
    Private(String),
}

fn render_scope(scope: Scope) -> String {
    return match scope {
        Scope::Public => "".to_string(),
        Scope::Private(inn) => format!("<private[{}]>", inn),
    };
}

fn scope(input: &str) -> Result<Scope, String> {
    //println!("scope: {}", input);
    if input.starts_with("O") {
        return Ok(Scope::Public);
    } else if input.starts_with("P") {
        return defn_name(&input[1..]).map(|i| return Scope::Private(i));
    } else {
        return Err(format!("scope: cannot read `{}`", input).to_string());
    }
}

fn name(input: &str) -> (usize, ParsingResult) {
    //println!("name: {}", input);
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
    use super::*;

    fn run(s: &str) -> String {
        return scala_native_demangle(s).unwrap();
    }

    #[test]
    fn it_works() {
        assert_eq!(run("_ST10__dispatch"), "__dispatch");
        assert_eq!(
            run("_SM42sttp.model.headers.CacheDirective$MinFreshD12productArityiEO"),
            "sttp.model.headers.CacheDirective$MinFresh.productArity: scala.Int"
        );

        assert_eq!(run("_SM42scala.scalanative.runtime.SymbolFormatter$D10inBounds$1L32scala.scalanative.unsigned.ULongizEPT42scala.scalanative.runtime.SymbolFormatter$"), 
            "scala.scalanative.runtime.SymbolFormatter$.inBounds$1(scala.scalanative.unsigned.ULong): scala.Boolean");

        assert_eq!(run("_SM42scala.scalanative.runtime.SymbolFormatter$D11readIdent$1L28scala.scalanative.unsafe.PtrL32scala.scalanative.unsigned.ULongL20scala.runtime.IntRefL28scala.scalanative.unsafe.PtruEPT42scala.scalanative.runtime.SymbolFormatter$"), 
            "scala.scalanative.runtime.SymbolFormatter$.readIdent$1(scala.scalanative.unsafe.Ptr,scala.scalanative.unsigned.ULong,scala.runtime.IntRef): scala.Unit");

        assert_eq!(run("_SM41scalaboot.template.scalatemplate$package$D10$anonfun$3L26scalaboot.template.ContextL15scala.Function1L23java.lang.StringBuilderL31scalaboot.template.UnsafeCursorL23scalaboot.template.MoveuEPT41scalaboot.template.scalatemplate$package$"), "")
    }
}
