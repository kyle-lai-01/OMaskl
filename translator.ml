open Ast

let ocaml_keywords = 
  ["and";"as";"assert";"asr";"begin";"class";"constraint";"do";"done";"downto";
   "end";"exception";"external";"for";"fun";"function";"functor";
   "in";"include";"inherit";"initializer";"land";"lazy";"let";"lor";"lsl";
   "lsr";"lxor";"match";"method";"mod";"module";"mutable";"new";"nonrec";
   "object";"of";"open";"or";"private";"rec";"sig";"struct";"to";
   "try";"type";"val";"virtual";"when";"with"] 

(** [dekeywordify var] appends an underscore to variable name if it conflicts 
    with an OCaml keyword.*)
let dekeywordify (var: string) :string = 
  if List.mem var ocaml_keywords then var ^ "_" else var

let rec translate_math_expr (expr : Ast.math_expr) =
  match expr with 
  | Integer int -> string_of_float (float_of_int int)
  | Double fl -> string_of_float fl
  | Mathvar (Var (n, _)) -> dekeywordify n
  | Mathfunc (f, args) -> 
    "(" ^ (translate_func f) ^ 
    (args |> List.map translate_expr 
     |> List.fold_left (fun x acc ->  x ^ " " ^ acc) "") ^ ")"
  | Binary (bin, exp1, exp2) -> translate_math_binary bin exp1 exp2
  | Unary (un, exp) -> 
    (translate_math_unary un) ^ (translate_math_expr exp) ^ ")"

and translate_math_binary bin exp1 exp2 =
  let str = 
    match bin with 
    | Plus -> 
      (translate_math_expr exp1) ^ " +. " ^ (translate_math_expr exp2)
    | Minus -> 
      (translate_math_expr exp1) ^ " -. " ^ (translate_math_expr exp2)
    | Times -> 
      (translate_math_expr exp1) ^ " *. " ^ (translate_math_expr exp2)
    | Divide -> 
      (translate_math_expr exp1) ^ " /. " ^ (translate_math_expr exp2)
    | Modulo -> 
      "(int_of_float " ^ (translate_math_expr exp1) ^ ") mod " ^ 
      "(int_of_float " ^ (translate_math_expr exp2) ^ ")"
    | Power -> "Float.pow " ^ (translate_math_expr exp1) ^ 
               " " ^ (translate_math_expr exp2)
    | Min -> "min " ^ (translate_math_expr exp1) 
             ^ " " ^ (translate_math_expr exp2)
    | Max -> "max " ^ (translate_math_expr exp1) 
             ^ " " ^ (translate_math_expr exp2)
  in "(" ^ str ^ ")"

and translate_math_unary un =
  let str = 
    match un with 
    | Sqrt -> "Float.sqrt "
    | Abs -> "Float.abs "
    | Floor -> "Float.floor "
    | Ceil -> "Float.ceil "
  in "(" ^ str

and translate_comparison expr = 
  match expr with 
  | Equal -> " = "
  | Less -> " < "
  | Greater -> " > "
  | Leq -> " <= "
  | Geq -> " >= "
  | Neq -> " <> "

and translate_bool_binary expr = 
  match expr with 
  | And -> " && "
  | Or -> " || "
  | _ -> failwith "Invalid AST construction"

and translate_bool_expr (expr : Ast.bool_expr) = 
  let str = 
    match expr with 
    | Boolean b -> string_of_bool b
    | Boolvar (Var (n, _)) -> dekeywordify n
    | Boolfunc (f, args) -> "(" ^ (translate_func f) ^ (
        args |> List.map translate_expr 
        |> List.fold_left (fun x acc ->  x ^ " " ^ acc) "") ^ ")"
    | Negation ex -> "not " ^ (translate_bool_expr ex)
    | Comparison (c, mexp1, mexp2) -> 
      (translate_math_expr mexp1) ^ (translate_comparison c) ^
      (translate_math_expr mexp2)
    | Boolbin (bin, bexp1, bexp2) -> begin match bin with 
        | Nand -> translate_bool_expr (Negation (Boolbin (And, bexp1, bexp2)))
        | Nor -> translate_bool_expr (Negation (Boolbin (Or, bexp1, bexp2)))
        | Xor -> " (fun x y -> (x || y) && (not (x && y))) " ^ 
                 (translate_bool_expr bexp1) ^ " " ^(translate_bool_expr bexp2) 
        | binar -> (translate_bool_expr bexp1) ^ (translate_bool_binary binar) 
                   ^ (translate_bool_expr bexp2)
      end
  in "(" ^ str ^ ")"

and translate_string_expr expr =
  match expr with
  | Stringvar (Var (n, _)) -> dekeywordify n
  | Stringfunc (f, args) -> 
    "(" ^ (translate_func f) ^ 
    (args |> List.map translate_expr 
     |> List.fold_left (fun x acc ->  x ^ " " ^ acc) "") ^ ")"
  | Strung s -> s
  | Concat (expr1, expr2) -> "(" ^ (translate_string_expr expr1) 
                             ^ " ^ " ^ (translate_string_expr expr2) ^ ")"

and translate_func_var var= 
  match var with 
  | Var (s1, _) -> (dekeywordify s1) ^ " "

and translate_func expr =
  match expr with
  | Anonymous (vs, e) ->  
    "(fun " ^ (List.fold_left (^) "" (List.map translate_func_var vs))
    ^ "-> " ^ (translate_expr e) ^ ")"
  | Named (Var(s, _)) -> 
    if s = "print" then "print_string" else 
    if s = "print_endl" then "print_endline" else dekeywordify s
  | Function (s1, s2, f) -> failwith "Invalid AST construction"
  | _ -> failwith "Object-Oriented not supported"

and translate_expr expr = match expr with 
  | Unitexp u -> "()"
  | Boolexp b -> translate_bool_expr b
  | Mathexp m -> translate_math_expr m
  | Stringexp s -> translate_string_expr s 
  | Conditional (b, e1, e2) -> 
    "if " ^ (translate_bool_expr b) ^ "\nthen " ^
    (translate_expr e1) ^ "\nelse " ^ (translate_expr e2)
  | Defvar (Var(s, _), e1, e2) -> 
    "let "^(dekeywordify s)^" = "^(translate_expr e1)^" in\n"^translate_expr e2
  | Func (f, args) -> "(" ^ (translate_func f) ^ (
      args |> List.map translate_expr 
      |> List.fold_left (fun x acc ->  x ^ " " ^ acc) "") ^ ")"
  | Deffunc (Function (s1, s2, f2), e) ->
    "let rec "^(dekeywordify s1)^" = "^(translate_func f2)^" in\n"^
    (translate_expr e)
  | Deffunc _ -> failwith "Invalid AST construction"
  | Funcval (f) -> translate_func f
  | Def d -> translate_def d
  | Deflst d -> translate_def_lst d
  | _ -> failwith "Object-Oriented not supported"

(** [translate_def_lst d] is the [string] of the [OCaml] code
    represented by the definition list [d]. *)
and translate_def_lst d = 
  match d with 
  | [] -> ""
  | h::t -> (translate_def h) ^ translate_def_lst t

(** [translate_def d] is the [string] of the [OCaml] code
    represented by the definition [d]. 
    Raises: [Failure "Object-Oriented not supported"] if [e]
    is an Object-Oriented function expression.  *)
and translate_def d = 
  match d with 
  | Vardef (Var(s, _), e) -> 
    "let " ^ (dekeywordify s) ^ " =\n" ^ (translate_expr e) ^ "\n\n"
  | Funcdef (Var(s, _), (Anonymous _ as e)) -> 
    "let rec " ^ (dekeywordify s) ^ " =\n" ^ (translate_func e) ^ "\n\n"
  | Funcdef (_, _) -> failwith "Invalid AST construction"
  | _ -> failwith "Object-Oriented not supported"