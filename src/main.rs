#![feature(cell_leak)]

use std::process::exit;

use runtime::evaluator::Evaluator;

mod errors;
mod frontend;
mod runtime;

fn main() {
    let args: Vec<_> = std::env::args().collect();

    let Some(file) = args.get(1) else {
        println!("Usage: peanut <file.pn>");
        exit(0);
    };

    let Ok(content) = std::fs::read_to_string(file) else {
        eprintln!("peanut can't open this file `{}`", file);
        exit(1);
    };

    Evaluator::evaluate(&content).unwrap();
}
