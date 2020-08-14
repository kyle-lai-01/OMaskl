let parse s = 
  let lex = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lex in 
  ast

let interp env e = 
  e |> parse |> Eval.eval_expr env