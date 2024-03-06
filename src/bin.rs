use std::{
    fs::File,
    io::{self, BufRead},
    path::Path,
};

use scala_native_demangle::{self, DemanglingConfig};

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
        debug: bool,
    },
    Id {
        name: String,
        #[arg(long)]
        debug: bool,
    },
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::File { name, debug } => {
            if let Ok(lines) = read_lines(name) {
                for line in lines {
                    if let Ok(ip) = line {
                        match scala_native_demangle::demangle(&ip, &Default::default()) {
                            Ok(res) => println!("{} = {}", ip, res),
                            Err(e) => println!("{} ERROR {}", ip, e),
                        }
                    }
                }
            }
        }
        Commands::Id { name, debug } => {
            println!(
                "{}",
                scala_native_demangle::demangle(
                    name,
                    &DemanglingConfig {
                        debug: *debug,
                        ..Default::default()
                    }
                )
                .unwrap()
            )
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
