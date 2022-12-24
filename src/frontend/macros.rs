#[macro_export]
macro_rules! parse_op_expr {
    ($(fn $name:ident from $calle:ident {$($token:path => $op:expr,)* })*) => {
        $(fn $name(&mut self) -> Result<Expr> {
            let mut left = self.$calle()?;

            while $(self.check($token).is_ok() ||)* false {
                let op = match self.eat().unwrap().kind() {
                    $(&$token => $op,)*
                    _ => panic!(),
                };

                let right = self.$calle()?;
                left = Expr::Op(op, Box::new(left), Box::new(right))
            }

            Ok(left)
        })*
    };
}
