{
  open Parser
}

let whitespace = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let bool = "true" | "false"
let float = '-'? digit+ '.'? digit+?
let varname = (['a'-'z'] | ['A'-'Z'] | digit | '_')+
let string = (['a'-'z'] | ['A'-'Z'] | digit | '_' | ' ')+


rule read = 
  parse 
  | whitespace { read lexbuf }
  | "()" {UNIT}
  | "in" {IN}
  | "int" {TINT}
  | "float" {TFLOAT}
  | "string" {TSTRING}
  | "boolean" {TBOOL}
  | "func" {TFUNC} 
  | "unit" {TUNIT}
  | "class" {TCLASS}
  | "constr" {CONSTR}
  | "." {PER}
  | "->_->" {ARROW}
  | ":=" {ASSIGN}
  | "=_=" {EQUAL}
  | "<_<" {LESS}
  | ">_>" {GREATER}
  | ">=_=>" {GEQ}
  | "<=_=<" {LEQ}
  | "!=_=!" {NEQ}
  | "+_+" {PLUS}
  | "-_-" {MINUS}
  | "*_*" {TIMES}
  | "/_/" {DIVIDE}
  | "%_%" {MODULO}
  | "^_^" {POWER}
  | "sqrt" {SQRT}
  | "abs" {ABS}
  | "floor" {FLOOR}
  | "ceil" {CEIL}
  | "min" {MIN}
  | "max" {MAX}
  | "!_!" {NOT}
  | "&_&" {AND}
  | "|_|" {OR}
  | "!_|" {NOR}
  | "!_&" {NAND}
  | "&_|" {XOR}
  | "$_$" {CONCAT}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "[" {LSQR}
  | "]" {RSQR}
  | "," {COMMA}
  | "{" {LCURL}
  | "}" {RCURL}
  | "(" {LPAREN} 
  | ")" {RPAREN} 
  | "<" {LANGLE}
  | ">" {RANGLE}
  | int { INT (int_of_string (Lexing.lexeme lexbuf))}
  | bool { BOOL (bool_of_string (Lexing.lexeme lexbuf))}
  | float { FLOAT ( Float.of_string (Lexing.lexeme lexbuf))}
  | varname { VARIABLE (Lexing.lexeme lexbuf) }
  | '\"' string '\"' { STRING (Lexing.lexeme lexbuf) }
  | eof {EOF}

  
 