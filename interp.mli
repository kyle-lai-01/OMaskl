(** Interpreting and parsing strings. *)

(** [parse s] is the abstract syntax tree obtained from lexing
    and parsing string [s]. *)
val parse : string -> Ast.expr

(** [interp env s] is the value obtained by evaluating the 
    expression produced from the OMaskl code in [s] in environment [env]. *)
val interp : Eval.env -> string -> Eval.value