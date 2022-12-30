#[macro_export]
macro_rules! parse_op_expr {
    ($(fn $name:ident from $calle:ident {$($token:path,)* })*) => {
        $(fn $name(&mut self) -> Result<Expr> {
            let mut left = self.$calle()?;

            while !self.check_index(1, TokenKind::Equal) && $(self.check($token))||* {
                let token = self.eat().unwrap();
                let op = token.try_into()?;

                let right = self.$calle()?;
                left = Expr::Op(op, Box::new(left), Box::new(right))
            }

            Ok(left)
        })*
    };
}
