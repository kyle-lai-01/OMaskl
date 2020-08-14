(** Evaluates OMaskl expressions. *)

open Ast

(** The type of values produced by evaluating OMaskl expressions. 
    [Integer i] is the value of int [i].
    [Double f] is the value of float [f]. 
    [Boolean b] is the value of boolean [b]. 
    [String s] is the value of string [s]. 
    [Closure vlst e env] is the value of a function with arguments [vlst] 
    body expression [e] and closure environment reference [env].
    [Environment env] is the value of environment [env]. 
    [Unit u] is the value of unit [u]. 
    [Class c vlst env] is the value of class variable [c] with fields [vlst] 
    and environment reference [env].
    [Object c v env] is the value of object [v] with class name [c] and 
    environment [env]*)
type value = 
  | Integer of int
  | Double of float
  | Boolean of bool
  | String of string
  | Closure of variable list * expr * env ref
  | Environment of env
  | Unit of unit
  | Class of variable * variable list * env ref
  | Object of string * value * env

(** The type of environments *)
and env = (string * value) list

(** [initialize_env] is the value of the initial environment. *)
val initialize_env : env

(** [string_of_value v] is the string representation of value [v]. *)
val string_of_value : value -> string

(** [eval_expr env e] is the value produced by evaluating 
    expression [e] in environment [env].  *)
val eval_expr : env -> Ast.expr -> value