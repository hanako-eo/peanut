use frontend::parser::Parser;

mod errors;
mod frontend;

fn main() {
    let parser = Parser::parse("0;");

    println!("{:?}", parser);
}
