%{ 
open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <float> FLOAT 
%token <string> STRING 
%token UNIT
%token TUNIT
%token TINT
%token TFLOAT
%token TBOOL
%token TSTRING
%token <string> VARIABLE
%token TFUNC
%token ASSIGN
%token LANGLE
%token RANGLE
%token TCLASS
%token CONSTR
%token PER
%token CONCAT
%token EQUAL
%token LESS
%token GREATER
%token GEQ
%token LEQ
%token NEQ
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MODULO
%token POWER
%token SQRT
%token ABS
%token FLOOR
%token CEIL
%token MAX
%token MIN
%token NOT
%token AND
%token OR
%token NOR
%token NAND
%token XOR
%token IF
%token THEN
%token ELSE
%token ARROW
%token LSQR
%token RSQR
%token COMMA
%token LCURL
%token RCURL
%token LPAREN
%token RPAREN
%token IN
%token EOF


%left NOT
%left OR NOR XOR
%left AND NAND
%left PLUS MINUS
%left TIMES DIVIDE MODULO
%left POWER
%left SQRT ABS FLOOR CEIL MIN MAX


%start <Ast.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | UNIT {Unitexp (())}
    | c = variable; PER; f = func_expr; LSQR; exprs = func_exprs; 
    RSQR { Classfunc (c, f, exprs) } 
    | c = variable; v = variable; ASSIGN; CONSTR; LSQR; exprs = func_exprs; 
    RSQR; IN; e = expr { Constructor (c, v, exprs, e) }
    | c = variable; v = variable; ASSIGN; e1 = expr; IN e2 = expr
     {Classvar (c, v, e1, e2)}
    | v = variable; PER; f = variable { Field(v, f) }
    | e = math_expr { Mathexp (e) }
    | e = bool_expr { Boolexp (e) }
    | e = string_expr { Stringexp (e) }
    | e = conditional_expr { e }
    | d = deflst {Deflst (d)}
    | d = def {Def (d)}
    | f = func_expr; e = expr { Deffunc(f, e) }
    | f = func_expr { Funcval(f) }
    | f = func_expr; LSQR; exprs = func_exprs; RSQR {Func (f, exprs)}
    | TINT; s = VARIABLE; ASSIGN; e1 = expr; IN; 
      e2 = expr { Defvar (Var (s, "int"), e1, e2)}
    | TFLOAT; s = VARIABLE; ASSIGN; e1 = expr; IN;
      e2 = expr { Defvar (Var (s, "float"), e1, e2)}
    | TBOOL; s = VARIABLE; ASSIGN; e1 = expr; IN;  
      e2 = expr { Defvar (Var (s, "boolean"), e1, e2)}
    | TSTRING; s = VARIABLE; ASSIGN; e1 = expr; IN; 
      e2 = expr { Defvar (Var (s, "string"), e1, e2)}
    | LPAREN; e = expr; RPAREN { e }
    ;

deflst:
  | d = def {[d]}
  | d = def; dlst = deflst {d::dlst}

def:
  | TINT; s = VARIABLE; ASSIGN; e1 = expr { Vardef (Var (s, "int"), e1) }
  | TFLOAT; s = VARIABLE; ASSIGN; e1 = expr { Vardef (Var (s, "float"), e1)}
  | TBOOL; s = VARIABLE; ASSIGN; e1 = expr { Vardef (Var (s, "boolean"), e1)}
  | TSTRING; s = VARIABLE; ASSIGN; e1 = expr { Vardef (Var (s, "string"), e1)}
  | TUNIT; s = VARIABLE; ASSIGN; e1 = expr { Vardef (Var (s, "unit"), e1)}
  | TFUNC; LANGLE; TINT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Funcdef (Var (s, "int"), e)}
  | TFUNC; LANGLE; TFLOAT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Funcdef (Var (s, "float"), e)}
  | TFUNC; LANGLE; TBOOL; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Funcdef (Var (s, "boolean"), e)}
  | TFUNC; LANGLE; TSTRING; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Funcdef (Var (s, "string"), e)}
  | TFUNC; LANGLE; TUNIT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Funcdef (Var (s, "unit"), e)}
  | TFUNC; LANGLE; c = variable; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr { Classfuncdef (s, c, e)}
  | TCLASS; c = variable; ASSIGN; LCURL; LSQR; fields = func_var_expr; 
      RSQR; d = deflst; RCURL {Classdef (c, fields, d)}
  | c = variable; v = variable; ASSIGN; CONSTR; LSQR; exprs = func_exprs; 
    RSQR { Constrdef (c, v, exprs) }
  | c = variable; v = variable; ASSIGN; e1 = expr
     {Classvardef (c, v, e1)}
  ;


variable: 
  | s = VARIABLE {Var (s, "")}
  ;

func_var_expr:
    | TINT; s = VARIABLE { [Var (s, "int")] }
    | TINT; s = VARIABLE; COMMA; e = func_var_expr {(Var (s, "int"))::e}
    | TFLOAT; s = VARIABLE { [Var (s, "float")] }
    | TFLOAT; s = VARIABLE; COMMA; e = func_var_expr {(Var (s, "float"))::e}
    | TBOOL; s = VARIABLE { [Var (s, "bool")] }
    | TBOOL; s = VARIABLE; COMMA; e = func_var_expr {(Var (s, "bool"))::e}
    | TSTRING; s = VARIABLE { [Var (s, "string")] }
    | TSTRING; s = VARIABLE; COMMA; e = func_var_expr {(Var (s, "string"))::e}
    | TUNIT; s = VARIABLE { [Var (s, "unit")] }
    | TUNIT; s = VARIABLE; COMMA; e = func_var_expr {(Var (s, "unit"))::e}
    | variable; s = VARIABLE { [Var (s, "")]}
    | variable; s = VARIABLE; COMMA; e = func_var_expr {(Var(s, "unit"))::e}
    | LPAREN; e = func_var_expr; RPAREN { e }
    ;

func_exprs: 
    | e = expr { [e] }
    | e = expr; COMMA; es = func_exprs {e::es}
    | LPAREN; e = func_exprs; RPAREN { e }
    ;

func_expr:
    | TFUNC; LANGLE; TINT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr; IN { Function (s, "int", e)}
    | TFUNC; LANGLE; TBOOL; RANGLE; s = VARIABLE; ASSIGN;
      e = func_expr; IN { Function (s, "boolean", e)}
    | TFUNC; LANGLE; TFLOAT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr; IN { Function (s, "float", e)}
    | TFUNC; LANGLE; TSTRING; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr; IN { Function (s, "string", e)}
    | TFUNC; LANGLE; TUNIT; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr; IN { Function (s, "unit", e)}
    | TFUNC; LANGLE; c = variable; RANGLE; s = VARIABLE; ASSIGN; 
      e = func_expr; IN {Funcclass (s, c, e)}
    | LSQR; vars = func_var_expr; RSQR; ARROW; LCURL; e = expr; 
      RCURL { Anonymous (vars, e)}
    | s = variable { Named (s) }
    | LPAREN; e = func_expr; RPAREN { e }
    ;

math_expr: 
    | i = INT { Integer i }
    | f = FLOAT { Double f }
    | s = variable { Mathvar (s) }
    | f = func_expr; LSQR; exprs = func_exprs; RSQR {Mathfunc (f, exprs)}
    | e1 = math_expr; TIMES; e2 = math_expr { Binary( Times, e1, e2 ) }
    | e1 = math_expr; DIVIDE; e2 = math_expr { Binary( Divide, e1, e2 )}
    | e1 = math_expr; PLUS; e2 = math_expr {Binary( Plus, e1, e2 )}
    | e1 = math_expr; MINUS; e2 = math_expr {Binary( Minus, e1, e2 )}
    | e1 = math_expr; MODULO; e2 = math_expr {Binary( Modulo, e1, e2 )}
    | e1 = math_expr; POWER; e2 = math_expr { Binary( Power, e1, e2 )}
    | MIN; e1 = math_expr; e2 = math_expr { Binary( Min, e1, e2 )}
    | MAX; e1 = math_expr; e2 = math_expr { Binary( Max, e1, e2 )}
    | SQRT; e1 = math_expr { Unary( Sqrt, e1)}
    | ABS; e1 = math_expr { Unary( Abs, e1) }
    | FLOOR; e1 = math_expr { Unary( Floor, e1)}
    | CEIL; e1 = math_expr { Unary( Ceil, e1) }
    | LPAREN; e = math_expr; RPAREN { e }
    ;

bool_expr:
    | b = BOOL { Boolean b }
    | s = variable { Boolvar (s) }
    | f = func_expr; LSQR; exprs = func_exprs; RSQR {Boolfunc (f, exprs)}
    | e1 = math_expr; EQUAL; e2 = math_expr { Comparison (Equal, e1, e2)}
    | e1 = math_expr; LESS; e2 = math_expr { Comparison (Less, e1, e2)}
    | e1 = math_expr; GREATER; e2 = math_expr { Comparison (Greater, e1, e2)}
    | e1 = math_expr; LEQ; e2 = math_expr { Comparison (Leq, e1, e2)}
    | e1 = math_expr; GEQ; e2 = math_expr { Comparison (Geq, e1, e2) }
    | e1 = math_expr; NEQ; e2 = math_expr { Comparison (Neq, e1, e2)}
    | NOT; e = bool_expr { Negation (e)}
    | e1 = bool_expr; AND; e2 = bool_expr { Boolbin (And, e1, e2)}
    | e1 = bool_expr; OR; e2 = bool_expr { Boolbin (Or, e1, e2)}
    | e1 = bool_expr; NOR; e2 = bool_expr { Boolbin (Nor, e1, e2)}
    | e1 = bool_expr; NAND; e2 = bool_expr { Boolbin (Nand, e1, e2)}
    | e1 = bool_expr; XOR; e2 = bool_expr { Boolbin (Xor, e1, e2)}
    | LPAREN; e = bool_expr; RPAREN { e }
    ;

string_expr:
    | s = STRING { Strung s }
    | f = func_expr; LSQR; exprs = func_exprs; RSQR {Stringfunc (f, exprs)}
    | s = variable { Stringvar (s) }
    | e1 = string_expr; CONCAT; e2 = string_expr { Concat(e1, e2)}
    | LPAREN; e = string_expr; RPAREN { e }
    ;

conditional_expr:
    | IF; LCURL; e1 = bool_expr; RCURL; THEN; e2 = expr; ELSE; 
      e3 = expr { Conditional(e1, e2, e3)}
    | LPAREN; e = conditional_expr; RPAREN { e }
    ;