open Ast

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

and env = (string * value) list

let initialize_env = []

(** [extend_env env x v] is environment [env] updated to 
    include variable name [x] mapped to value [v]. *)
let rec extend_env env x v = 
  match env with 
  | [] -> [(x, v)]
  | h::t -> if fst h = x then extend_env t x v
    else h::extend_env t x v

(** [find_in_env env x] is the value bound to variable name [x] in 
    environment [env]. 
    Raises: [Failure "No such value"] if [x] is not in the environment.  *)
let rec find_in_env env x = 
  match env with
  | [] -> failwith "No such value"
  | h::t -> if fst h = x then snd h else find_in_env t x

(** [combine_envs env1 env2] is environment [env2] updated
    with the mappings of environment [env1].  *)
let rec combine_envs env1 env2 = 
  match env1 with
  | [] -> env2
  | (x, v)::t -> combine_envs t (extend_env env2 x v)

(** [string_of_env env] is the string representation of environment [env].  *)
and string_of_env env = 
  "{" ^ (env_string_helper env "") ^ "}"

(** [env_string_helper] is the string representation
     of the variable-value mappings in environment [env].  *)
and env_string_helper env acc = 
  match env with 
  | [] -> acc
  | (i, v)::t -> if acc = "" then 
      env_string_helper t (i ^ " : " ^ (string_of_value v) ^ acc)
    else env_string_helper t (i ^ " : " ^ (string_of_value v) ^ ", " ^ acc)

and string_of_value v = 
  match v with 
  | Integer i -> "int : " ^ string_of_int i
  | Double f -> "float : " ^ string_of_float f
  | Boolean b -> "boolean : " ^ string_of_bool b
  | String s -> "string : \"" ^ s ^ "\""
  | Closure _ -> "function : <func>"
  | Environment e -> "environment : " ^ string_of_env e
  | Unit u -> "unit : ()"
  | Class (n, vlst, env) -> "class : <class>"
  | Object (s, c, vals) -> "object : <obj>"

let rec eval_expr env e = 
  match e with 
  | Unitexp u -> Unit u
  | Mathexp e1 -> eval_math env e1
  | Boolexp e1 -> eval_bool env e1
  | Stringexp e1 -> eval_string env e1
  | Conditional (b, e1, e2) -> eval_cond env b e1 e2
  | Defvar (v, e1, e2) -> eval_var_exp env v e1 e2
  | Deffunc (f, e) -> eval_func_dec env f e
  | Func (f, elst) -> eval_app env f elst
  | Funcval f -> eval_func env f
  | Deflst d -> eval_program env d
  | Def d -> eval_def env d
  | Classfunc (Var(s,_), f, elst) -> eval_class_func env s f elst
  | Constructor (Var (s,_), Var(n,_), elst, e1) -> eval_constr env s n elst e1
  | Classvar (c, Var (v, _), e2, e1) -> eval_classvar env c v e2 e1
  | Field (Var(o, _), Var(v, _)) -> eval_field env o v

(** [eval_field env o v] is the value of field [v] of object [o]
    in the environment [env]. 
    Raises: [Failure "Not an object"] if [o] is not bound to 
    an object value.  *)
and eval_field env o v = 
  begin match find_in_env env o with
    | Object (_, _, env_cl) -> find_in_env env_cl v
    | _ -> failwith "Not an object"
  end

(** [eval_classvar env c var_name e2 e1] is the evaluation of [e1] in 
    the environment [env] extended with the value of [e2] bound to 
    [var_name] which is a variable of class type [c]. *)
and eval_classvar env c var_name e2 e1 =
  let obj = eval_expr env e2 in 
  let env_upd = extend_env env var_name obj in eval_expr env_upd e1

(** [eval_class_func env s f elst] is the value of applying 
    function [f], defined in the class bound to [s] in 
    environment [env], to the arguments in [elst]. 
    Raises: [Failure "Not a class name"] if [s] is not bound
    to a class value. *)
and eval_class_func env s f elst = 
  let new_env = begin match find_in_env env s with 
    | Class (v, vlst, env_cl) -> combine_envs !env_cl env
    | _ -> failwith "Not a class name" end in eval_app new_env f elst

(** [eval_constr env s n elst e1] is the value of [e1] evaluated in 
    the environment [env] extended with the object created with the
    constructor applied on arguments [elst] bound to variable [n] 
    with class type [s].
    Raises: [Failure "Not a class"] if [s] is not bound to a 
    class value.  *)
and eval_constr env s n elst e1 = 
  let cls = find_in_env env s in
  let vars = begin match cls with
    | Class (v, vlst, env) -> vlst
    | _ -> failwith "Not a class" end in 
  let new_env = update_args env env vars elst in 
  let env_upd = extend_env env n (Object (n, cls, new_env)) in 
  eval_expr env_upd e1

(** [eval_def env d] is the value produced by evaluating definition [d]
    in environment [env]. *)
and eval_def env d = 
  match d with 
  | Vardef(Var(str,_),e)-> Environment (extend_env env str (eval_expr env e))
  | Funcdef (Var(str,_), f) -> eval_func_def env str f
  | Classdef (Var (str, _) as v, vlst, flst) -> eval_class_def env v vlst flst
  | Classfuncdef (s, v, f) -> eval_class_func_def env s v f
  | Constrdef (c, Var (v, _), elst) -> eval_constr_def env c v elst
  | Classvardef (c, Var(v, _), e1) -> eval_class_var_def env c v e1

(** [eval_class_var_def env c var_name e1] is the value containing the extended
    environment [env] with the value of [e1] bound to variable [var_name] 
    with class type [c].  *)
and eval_class_var_def env c var_name e1 = 
  let obj = eval_expr env e1 in 
  let env_upd = extend_env env var_name obj in 
  Environment env_upd

(** [eval_constr_def env c var_name elst] is the value containing the extended
    environment [env] with the value of applying the constructor to 
    the arguments [elst] bound to variable [var_name] with class type [c].
    Raises: [Failure "Not a class"] if [c] is not bound to a
    class value. *)
and eval_constr_def env c var_name elst = 
  let cls = begin match c with 
    | Var (s, _) -> find_in_env env s
  end in 
  let vars = begin match cls with
    | Class (v, vlst, env) -> vlst
    | _ -> failwith "Not a class"
  end in 
  let new_env = update_args env env vars elst in 
  let obj = Object (var_name, cls, new_env) in 
  let env_upd = extend_env env var_name obj in 
  Environment env_upd

(** [eval_class_func_def env s v f] is the value containing the extended
    environment [env] with the value of function [f] bound to variable [v]
    with class type [s]. 
    Raises: [Failure "Not a function"] if [f] does not evaluate to 
    a function closure. *)
and eval_class_func_def env s v f = 
  let v1 = eval_func env f in 
  let env_updated = extend_env env s v1 in 
  begin match v1 with 
    | Closure (s, e1, env_cl) -> env_cl := env_updated;
      Environment (!env_cl)
    | _ -> failwith "Not a function"
  end 

(** [eval_class_def env v vlst flst] is the value containing the extended
    environment [env] with the value of the class containing fields [vlst]
    and function definitions [flst] bound to class variable [v]. 
    Raises: [Failure "Not a valid class definition"] if [flst] does not
    evaluate to an environment value. *)
and eval_class_def env v vlst flst = 
  let str = begin match v with 
    | Var (s, _) -> s 
  end in 
  let env_upd =  begin match eval_program env flst with 
    | Environment en -> en
    | _ -> failwith "Not valid class definition"
  end in 
  let env_cl = ref env_upd in 
  let cls = Class (v, vlst, env_cl) in 
  let new_env = extend_env !env_cl str cls in 
  env_cl := new_env; 
  let env_upd2 =  begin match eval_program !env_cl flst with 
    | Environment en -> en
    | _ -> failwith "Not valid class definition"
  end in env_cl := env_upd2; Environment (extend_env env str cls)

(** [eval_func_def env str f] is the value containing the extended
    environment [env] with the function [f] bound to variable [str].
    Raises: [Failure "Not a function"] if [f] does not 
    evaluate to a function closure.  *)
and eval_func_def env str f = 
  let v1 = eval_func env f in 
  let env_updated = extend_env env str v1 in 
  begin match v1 with 
    | Closure (s, e1, env_cl) -> env_cl := env_updated; Environment (!env_cl)
    | _ -> failwith "Not a function" end 

(** [eval_func_dec env f e] is the value produced by evaluating [e]
    in the environment [env] extended to include the function in function
    expression [f] bound to the variable name inside [f].
    Raises: [Failure "Not a function"] if the function in [f] does
    not evaluate to a function closure
    Raises: [Failure "Invalid function definition"] if [f]
    is an invalid definition of a function. *)
and eval_func_dec env f e = 
  match f with 
  | Function (s1, _, f1) ->
    let v1 = eval_func env f1 in 
    let env_updated = extend_env env s1 v1 in 
    begin match v1 with 
      | Closure (s, e1, env_cl) -> env_cl := env_updated;
        eval_expr !env_cl e
      | _ -> failwith "Not a function"
    end 
  | _ -> failwith "Invalid function definition"

(** [eval_func env e] is the value of function [e] evaluated in
    environment [env].
    Raises: [Failure "Invalid function definition"] if [e] 
    does not evaluate to a function closure.  *)
and eval_func env e = 
  match e with 
  | Anonymous (vlist, e1) -> Closure (vlist, e1, ref env)
  | Named v -> eval_var env v
  | _ -> failwith "Invalid function definition"

(** [eval_app env f elst] is the value of function [f] applied
    to the arguments in expression list [elst].
    If [f] is the named function [print], print the value of 
    the expression in [elst]. 
    If [f] is the named function [print_endl], print the value of
    the expression in [elst] concatenated with the end line escape character. 
    Raises: [Failure "Arg mismatch"] if the list of arguments [elst]
    does not match the list required by function [f].
    Raises: [Failure "Not a function"] if [f] does not 
    evaluate to a function closure and is not [print] or
    [print_endl]. *)
and eval_app env f elst = 
  match f with 
  | Named (Var (s, _)) when s = "print" ->
    begin match elst with 
      | h::[] -> Unit (print_string (string_of_value (eval_expr env h)))
      | _ -> failwith "Arg mismatch"
    end 
  | Named (Var (s, _)) when s = "print_endl" -> 
    begin match elst with 
      | h::[] -> Unit (print_endline (string_of_value (eval_expr env h)))
      | _ -> failwith "Arg mismatch"
    end 
  | _ -> begin match eval_func env f with 
      | Closure (slst, e, env_cl) -> 
        let new_env = update_args env !env_cl slst elst in 
        eval_expr new_env e
      | _ -> failwith "Not a function"
    end 

(** [update_args ienv env slst elst] is environment [env] updated
    with the value of each expression in [elst] evaluated in
    environment [ienv] bound to the corresponding variable in [slst].
    Raises: [Failure "Arg mismatch"] if the expressions in [elst] 
    do not match the variables in [slst].  *)
and update_args ienv env slst elst = 
  match slst, elst with 
  | [], [] -> env
  | (Var (hs, _))::ts, he::te -> let v = (eval_expr ienv he) in 
    update_args ienv (extend_env env hs v) ts te
  | _ -> failwith "Arg mismatch"

(** [eval_var_exp env v e1 e2] is the value of 
    [e2] evaluated in environment [env] extended with the value
    of expression [e1] bound to variable [v]. *)
and eval_var_exp env v e1 e2 = 
  let v1 = eval_expr env e1 in 
  let env_updated = begin match v with 
    | Var (s, t) -> extend_env env s v1
  end in eval_expr env_updated e2

(** [eval_cond env be e1 e2] is the value of [if be then e1 else e2]
    evaluated in environment [env]. 
    Raises: [Failure "If guard error"] if [be] does not 
    evaluate to a boolean value.  *)
and eval_cond env be e1 e2 = 
  match eval_bool env be with
  | Boolean b -> if b then eval_expr env e1 else eval_expr env e2
  | _ -> failwith "If guard error"

(** [eval_string env e] is the value of string expression [e] in
    environment [env]. 
    Raises: [Failure "Invalid string"] if [e] does not 
    evaluate to a valid string value delimited by quotations.
    Raises: [Failure "Invalid concatenation"] if
    [e] is an invalid concatenation expression.  *)
and eval_string env e =
  match e with 
  | Stringvar v -> eval_var env v 
  | Stringfunc (f, elst) -> eval_app env f elst
  | Strung s -> let slist = String.split_on_char '\"' s in 
    begin match slist with 
      | _::str::_ -> String str
      | _ -> failwith "Invalid string"
    end
  | Concat (e1, e2) -> 
    begin match eval_string env e1, eval_string env e2 with
      | String s1, String s2 -> String (s1 ^ s2)
      | _ -> failwith "Invalid concatenation"
    end

(** [eval_bool env e] is the value of boolean expression [e] 
    evaluated in environment [env].  *)
and eval_bool env e = 
  match e with
  | Boolvar v -> eval_var env v
  | Boolfunc (f, elst) -> eval_app env f elst
  | Boolean b -> Boolean b
  | Negation e1 -> eval_negation env e1
  | Comparison (c, m1, m2) -> eval_compare env c m1 m2
  | Boolbin (bin, e1, e2) -> eval_boolbin env bin e1 e2

(** [eval_negation env e1] is the value of the negation 
    of boolean expression [e1] evaluated in environment [env]. 
    Raises: [Failure "Invalid boolean evaluation"] if [e1] does not 
    evaluate to a boolean value.  *)
and eval_negation env e1 =
  match eval_bool env e1 with 
  | Boolean b -> Boolean (not b)
  | _ -> failwith "Invalid boolean evaluation"

(** [eval_bincomp_int c] is the integer comparison represented by
    binary comparison operator [c].  *)
and eval_bincomp_int c = 
  match c with 
  | Equal -> (=)
  | Less -> (<)
  | Greater -> (>)
  | Leq -> (<=)
  | Geq -> (>=)
  | Neq -> (<>)

(** [eval_bincomp_flt c] is the float comparison represented by
    binary comparison operator [c].  *)
and eval_bincomp_flt c = 
  match c with 
  | Equal -> (=)
  | Less -> (<)
  | Greater -> (>)
  | Leq -> (<=)
  | Geq -> (>=)
  | Neq -> (<>)

(** [eval_compare env c m1 m2] is the value produced by evaluating the 
    comparison of math expression [m1] and math expression [m2] with 
    comparison operator [c] in environment [env]. 
    Raises: [Failure "Invalid comparison"] if [c] is not 
    a valid comparison operator or if [m1] or [m2] do not 
    evaluate to math values. *)
and eval_compare env c m1 m2 = 
  let (v1, v2) = begin match eval_math env m1, eval_math env m2 with
    | Integer i1, Integer i2 -> (Integer i1, Integer i2)
    | Integer i, Double f  -> (Double (float_of_int i), Double f)
    | Double f, Integer i -> (Double f, Double (float_of_int i))
    | Double f1, Double f2 -> (Double f1, Double f2)
    | _ -> failwith "Invalid comparison"
  end in 
  match v1, v2 with 
  | Integer i1, Integer i2 -> Boolean ((eval_bincomp_int c) i1 i2)
  | Double f1, Double f2 -> Boolean ((eval_bincomp_flt c) f1 f2)
  | _ -> failwith "Invalid comparison"

(** [eval_boolbin env bin e1 e2] is the value produced by evaluating 
    [e1 bin e2] in environment [env], where [e1] and [e2] are
    boolean expressions and [bin] is a boolean binary operator. 
    Raises: [Failure "Invalid boolean binary use"] if 
    [bin] is not a valid binary boolean operation or
    if [e1] or [e2] do not evaluate to boolean values. *)
and eval_boolbin env bin e1 e2 =
  match bin, eval_bool env e1, eval_bool env e2 with
  | And, Boolean b1, Boolean b2 -> Boolean (b1 && b2)
  | Or, Boolean b1, Boolean b2 -> Boolean (b1 || b2)
  | Nor, Boolean b1, Boolean b2 -> Boolean (not (b1 || b2))
  | Nand, Boolean b1, Boolean b2 -> Boolean (not (b1 && b2))
  | Xor, Boolean b1, Boolean b2 -> Boolean ((b1 || b2) && (not (b1 && b2)))
  | _ -> failwith "Invalid boolean binary use"

(** [eval_math env e] is the value of math expression [e] evaluated in 
    environment [env].  *)
and eval_math env e = 
  match e with 
  | Integer i -> Integer i
  | Double f -> Double f
  | Unary (u, e1) -> eval_unary env u e1
  | Binary (b, e1, e2) -> eval_binary env b e1 e2
  | Mathvar v ->  eval_var env v
  | Mathfunc (f, elst) -> eval_app env f elst

(** [eval_var env v] is the value bound to variable [v] in 
    environment [env].  *)
and eval_var env v =
  match v with 
  | Var (s, t) -> find_in_env env s

(** [binop b] is the binary float function represented by 
    binary math operation [b].  *)
and binop b = 
  match b with 
  | Plus -> (+.)
  | Minus -> (-.)
  | Times -> ( *. )
  | Divide -> ( /. )
  | Modulo -> (fun x y ->float_of_int ((int_of_float x) mod (int_of_float y)))
  | Power -> Float.pow 
  | Min -> Float.min
  | Max -> Float.max

(** [eval_binary env b e1 e2] is the value of applying 
    binary operation [b] to the value of math expressions [e1] and [e2] in 
    environment [env].  
    Raises: [Failure "Invalid binary operation use"] if 
    [b] is not a valid binary operation or [e1] or [e2] do not
    evaluate to math values.  *)
and eval_binary env b e1 e2 =
  let (v1, v2) = begin match eval_math env e1, eval_math env e2 with
    | Integer i1, Integer i2 -> (Integer i1, Integer i2)
    | Integer i, Double f  -> (Double (float_of_int i), Double f)
    | Double f, Integer i -> (Double f, Double (float_of_int i))
    | Double f1, Double f2 -> (Double f1, Double f2)
    | _ -> failwith "Invalid binary operation use"
  end in 
  match v1, v2 with
  | Integer i1, Integer i2 -> 
    let f1, f2 = float_of_int i1, float_of_int i2 in
    Integer (int_of_float ((binop b) f1 f2))
  | Double f1, Double f2 -> 
    Double ((binop b) f1 f2)
  | _ -> failwith "Invalid binary operation use"

(** [eval_unary env u e] is the value of applying math unary operation [u]
    to the value of expression [e]. 
    Raises: [Failure "Invalid unary operator use"] if [u] is not
    a valid unary operator or [e] does not evaluate to a math value. *)
and eval_unary env u e =
  match u, eval_math env e with 
  | Sqrt, Integer i -> Double (Float.sqrt (float_of_int i))
  | Sqrt, Double f -> Double (Float.sqrt f)
  | Abs, Integer i -> Integer (Int.abs i)
  | Abs, Double f -> Double (Float.abs f)
  | Floor, Integer i -> Integer i 
  | Floor, Double f -> Integer (int_of_float (Float.floor f))
  | Ceil, Integer i -> Integer i
  | Ceil, Double f -> Integer (int_of_float (Float.ceil f))
  | _ -> failwith "Invalid unary operator use"

(** [eval_program env dlist] is the value containing the environment
    produced by evaluating the definitions in [dlist] in environment [env]. 
    Raises: [Failure "Not a valid program"] if a definition in [dlist]
    is not a valid definition.  *)
and eval_program env dlist = 
  match dlist with 
  | [] -> Environment env 
  | h::t -> let env_updated = eval_def env h in 
    begin match env_updated with 
      | Environment e -> eval_program e t
      | _ -> failwith "Not a valid program"
    end