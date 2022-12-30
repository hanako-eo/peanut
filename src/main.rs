use runtime::evaluator::Evaluator;

mod errors;
mod frontend;
mod runtime;

fn main() {
    let value = Evaluator::evaluate("let a = 1; a");

    println!("{:#?}", value);
}
