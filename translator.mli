(** Translates OMaskl expressions into OCaml *)

open Ast

(** [translate_math_expr e] is the [string] of the [OCaml] code represented
    by math expression [e]. *)
val translate_math_expr : Ast.math_expr -> string

(** [translate_math_binary bin e1 e2] is the [string] of the [OCaml] code 
    represented by the math binary operation [bin] applied to 
    math expressions [e1] and [e2]. *)
val translate_math_binary : 
  Ast.math_binary -> Ast.math_expr -> Ast.math_expr -> string

(** [translate_math_unary un] is the [string] of the [OCaml] code
    reperesented by the math unary operation [un].*)
val translate_math_unary : Ast.math_unary -> string

(** [translate_comparison com] is the [string] of the [OCaml] code
    represented by the boolean comparison operator [com] *)
val translate_comparison : Ast.bool_comparison -> string

(** [translate_bool_binary e] is the [string] of the [OCaml] code
    represented by the binary boolean operation [e]. 
    Raises: [Failure "Invalid AST construction"] if the boolean binary
    operation in the abstract sytax tree is not properly constructed. *)
val translate_bool_binary : Ast.bool_binary -> string

(** [translate_bool_expr e] is the [string] of the [OCaml] code
    reperesented by the binary boolean operation [e]. 
    Raises: [Failure "Invalid AST construction"] if the boolean expression 
    in the abstract syntax tree is not properly constructed. *)
val translate_bool_expr : Ast.bool_expr -> string 

(** [translate_string_expr e] is the [string] of the [OCaml] code
    represented by the string expression [e]. *)
val translate_string_expr : Ast.string_expr -> string

(** [translate_func_var var] is the [string] of the [OCaml] code
    represented by the function variable [var]. *)
val translate_func_var : Ast.variable -> string

(** [translate_func e] is the [string] of the [OCaml] code
    represented by the function expression [e].
    Raises: [Failure "Invalid AST construction"] if the function expression 
    in the abstract syntax tree is not properly constructed. 
    Raises: [Failure "Object-Oriented not supported"] if [e]
    is an Object-Oriented function expression. *)
val translate_func : Ast.func_expr -> string

(** [translate_expr e] is the [string] of the [OCaml] code
    represented by the expression [e].
    Raises: [Failure "Invalid AST construction"] if the expression in the 
    abstract syntax tree is not properly constructed. 
    Raises: [Failure "Object-Oriented not supported"] if [e]
    is an Object-Oriented function expression. *)
val translate_expr : Ast.expr -> string

(** [ocaml_keywords] is a list of OCaml keywords that need to be replaced
    when used as variable names. *)
val ocaml_keywords : string list