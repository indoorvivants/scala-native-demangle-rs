use std::{
    fs::File,
    io::{self, BufRead},
    path::Path,
};

use scala_native_demangle::scala_native_demangle;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Process a file of mangled identifiers, outputting results inline, separating mangled/unmangled names via ` = `
    File {
        name: String,
    },
    Id {
        name: String,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::File { name } => {
            if let Ok(lines) = read_lines(name) {
                for line in lines {
                    if let Ok(ip) = line {
                        match scala_native_demangle::demangle(&ip) {
                            Ok(res) => println!("{} = {}", ip, res),
                            Err(e) => println!("{} ERROR {}", ip, e),
                        }
                    }
                }
            }
        }
        Commands::Id { name } => {
            println!("{}", scala_native_demangle::demangle(name).unwrap())
        }
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
