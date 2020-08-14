
type variable = 
  | Var of string * string


and math_binary =
  | Plus | Minus | Times | Divide | Modulo | Power | Min | Max

and math_unary =
  | Sqrt | Abs | Floor | Ceil

and math_expr = 
  | Mathvar of variable
  | Mathfunc of func_expr * (expr list)
  | Integer of int
  | Double of float
  | Unary of math_unary * math_expr
  | Binary of math_binary * math_expr * math_expr



and bool_comparison =
  | Equal | Less | Greater | Leq | Geq | Neq

and bool_binary = 
  | And | Or | Nor | Nand | Xor

and bool_expr = 
  | Boolvar of variable
  | Boolfunc of func_expr * (expr list)
  | Boolean of bool
  | Negation of bool_expr
  | Comparison of bool_comparison * math_expr * math_expr
  | Boolbin of bool_binary * bool_expr * bool_expr



and string_expr =
  | Stringvar of variable
  | Stringfunc of func_expr * (expr list)
  | Strung of string
  | Concat of string_expr * string_expr


and expr =
  | Unitexp of unit
  | Defvar of variable * expr * expr
  | Funcval of func_expr
  | Deffunc of func_expr * expr
  | Func of func_expr * (expr list)
  | Boolexp of bool_expr
  | Mathexp of math_expr
  | Stringexp of string_expr
  | Conditional of bool_expr * expr * expr
  | Deflst of def list
  | Def of def
  | Classfunc of variable * func_expr * expr list
  | Constructor of variable * variable * expr list * expr
  | Classvar of variable * variable * expr * expr
  | Field of variable * variable


and def = 
  | Vardef of variable * expr
  | Funcdef of variable * func_expr
  | Classfuncdef of string * variable * func_expr
  | Classvardef of variable * variable * expr
  | Constrdef of variable * variable * expr list
  | Classdef of variable * variable list * def list

and func_expr =
  | Anonymous of (variable list) * expr
  | Named of variable 
  | Function of string * string * func_expr
  | Funcclass of string * variable * func_expr

