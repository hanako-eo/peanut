use frontend::parser::Parser;

mod errors;
mod frontend;

fn main() {
    let parser = Parser::parse("if 2 1 else if 4 3;");

    println!("{:#?}", parser);
}
