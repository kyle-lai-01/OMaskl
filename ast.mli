(** The abstract syntax tree of OMaskl. *)

(** The type of variable expressions. 
    [Var (s1, s2)] is a variable expression where [s1] is the name
    of the variable and [s2] is the type of the variable*)
type variable = 
  | Var of string * string

(** The type of binary math operations. 
    [Plus] is the [+.] operation for [float] and [+] operation for [int].
    [Minus] is the [-.] operation for [float] and [-] operation for [int].
    [Times] is the [*.] operation for [float] and [*] operation for [int].
    [Divide] is the [/.] operation for [float] and [/] operation for [int].
    [Modulo] is the [mod] operation. 
    [Power] is the exponentiation operation. 
    [Min] is the minimum operation. 
    [Max] is the maximum operation. *)
and math_binary =
  | Plus | Minus | Times | Divide | Modulo | Power | Min | Max

(** The type of unary math operations. 
    [Sqrt] is the square root operation. 
    [Abs] is the absolute value operation. 
    [Floor] is the floor operation. 
    [Ceil] is the ceiling operation.*)
and math_unary =
  | Sqrt | Abs | Floor | Ceil

(** The type of math expressions. 
    [Mathvar v] is a Math variable where variable [v] contains the 
    name and type.
    [Mathfunc f elst] is the application of math function [f] 
    on a list of the function arguments [elst].
    [Integer i] is the int value [i]. 
    [Double f] is the float value [f]. 
    [Unary (u, e)] is the math expression with unary operation [u] applied to  
    math expression [e].
    [Binary (b, e1, e2)] is the math expression with binary operation [b] 
    applied to math expressions [e1] and [e2] *)
and math_expr = 
  | Mathvar of variable
  | Mathfunc of func_expr * (expr list)
  | Integer of int
  | Double of float
  | Unary of math_unary * math_expr
  | Binary of math_binary * math_expr * math_expr



(** The type of boolean comparisons of expressions. 
    [Equal] is the [=] comparison. 
    [Less] is the [<] comparison.
    [Greater] is the [>] comparison. 
    [Leq] is the [<=] comparison. 
    [Geq] is the [>=] comparison. 
    [Neq] is the [<>] comparison.  *)
and bool_comparison =
  | Equal | Less | Greater | Leq | Geq | Neq

(** The type of binary boolean operations. 
    [And] is the [&&] operation. 
    [Or] is the [||] operation. 
    [Nor] is the negation of the [||] operation. 
    [Nand] is the negation of the [&&] operation.
    [Xor] is the exclusive-or operation. *)
and bool_binary = 
  | And | Or | Nor | Nand | Xor

(** The type of boolean expressions. 
    [Boolvar v] is a boolean variable where variable [v] contains the 
    name and type.
    [Boolfunc f elst] is the application of boolean function [f] 
    on a list of the function arguments [elst].
    [Boolean b] is the bool value [b]. 
    [Negation b] is the negation of the boolean expression [b]
    [Comparison (c, e1, e2)] is the comparison [c] of
    math expression [e1] and math expression [e2].
    [Boolbin (b, e1, e2)] is the math expression with binary operation [b] 
    applied to boolean expressions [e1] and [e2]  *)
and bool_expr = 
  | Boolvar of variable
  | Boolfunc of func_expr * (expr list)
  | Boolean of bool
  | Negation of bool_expr
  | Comparison of bool_comparison * math_expr * math_expr
  | Boolbin of bool_binary * bool_expr * bool_expr



(** The type of string expressions. 
    [Stringvar v] is a string variable where variable [v] contains the 
    name and type.
    [Stringfunc f elst] is the application of string function [f] 
    on a list of the function arguments [elst].
    [Strung s] is the string value [s]. 
    [Concat (e1, e2)] is the concat operation applied to 
    string expressions [e1] and [e2] *)
and string_expr =
  | Stringvar of variable
  | Stringfunc of func_expr * (expr list)
  | Strung of string
  | Concat of string_expr * string_expr


(** The type of expressions. 
    [Unitexp u] is the unit value [u]
    [Defvar v e1 e2] is a creation of variable where variable [v] 
    contains the name and type, [e1] is bound to [v] in expression [e2].
    [Funcval f] is the function value [f]
    [Deffunc f e] is the defintion of function expression [f] 
    in expression [e]. 
    [Func f e_lst] is the application of function expression [f] on
    argument expressions in expression list [e_lst]. 
    [Boolexp e] is the boolean expression [e].
    [Mathexp e] is the math expression [e].
    [Stringexp e] is the string expression [e].
    [Conditional b e1 e2] is the [if] expression [if b then e1 else e2]. 
    [Deflst dlst] is a list of definitions [dlst].
    [Def d] is a definition [d].
    [Classfunc c f elst] is the appplication of class function expression [f] 
    defined in class [c] on argument expressions in expression list [elst].
    [Constructor c v elst e] is the constructor of a new object of class [c] 
    where the object created by the constructor called with arguments [elst] 
    is bound to variable [v] in expression [e].
    [Classvar c v e1 e2] is the creation of object variable [v] of class [c]
    where expression [e1] is bound to [v] in expression [e2]. 
    [Field v f] is the field [f] of object variable [v].*)
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

(** The type of definitions. 
    [Vardef v e] is the definition binding [e] to variable [v].
    [Funcdef v f] is the definition binding [f] to variable [v].
    [Classfuncdef s v f] is the definition binding [f] to variable [v] where
    [f] is a function with class type [s].
    [Classvardef c v e] is the definition binding [e] to variable [v] where
    [e] is an expression that evaluates to an object of type [c].
    [Constrdef c v elst] is the definition binding the object created
    by a call on the constructor with arguments [elst] to variable [v] where
    the object created is with class type [c].
    [classdef v vlst dlst] is the definition binding the class with 
    fields [vlst] and function definitions [dlst] to class type [v]. *)
and def = 
  | Vardef of variable * expr
  | Funcdef of variable * func_expr
  | Classfuncdef of string * variable * func_expr
  | Classvardef of variable * variable * expr
  | Constrdef of variable * variable * expr list
  | Classdef of variable * variable list * def list

(** The type of function expressions. 
    [Anonymous var_lst e] is the definition of an anonymous function with
    a list of arguments [var_lst] defined by expression [e]. 
    [Named s] is the function expression used for application of a function
    with name [s]. 
    [Function s1 s2 e] is the definition of a function with
    name [s1], type [s2] and function expression [e].
    [Funcclass c v f] is the function definition binding function [f] to 
    variable [v] with class type [s].*)
and func_expr =
  | Anonymous of (variable list) * expr
  | Named of variable
  | Function of string * string * func_expr
  | Funcclass of string * variable * func_expr