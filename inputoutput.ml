open Ast

let rec read_file (lst : string list) channel =
  try 
    channel |> input_line |> add_to_lst lst channel
  with End_of_file -> close_in channel;
    lst

and add_to_lst (lst : string list) channel (str : string) = 
  read_file (lst @ [str ^ " "]) channel



let translate_prog (p : Ast.expr) : string = 
  let prog = Translator.translate_expr p in prog


let rec output lst file_name = 
  let expr = List.fold_left (^) "" lst |> Interp.parse in 
  let string = begin match expr with 
    | Def _ -> expr |> translate_prog
    | Deflst _ -> expr |> translate_prog
    | _ -> "let main () = \n" ^ (expr |> translate_prog) ^ "\n\n" ^ 
           "let () = (fun x -> ignore (main ()); ()) ()"
  end in 
  let new_file_name = match String.split_on_char '.' file_name with 
    | h::t -> h ^ "_output.ml"
    | _ -> failwith "Not a .txt file"
  in 
  let out_chan = open_out new_file_name in 
  output_string out_chan string; close_out out_chan;
  new_file_name

(** [interp_output lst] prints the [string] of [OCaml] code translated
    from the [OMaskl] code stored in list [lst] *)
let rec interp_output lst = 
  let string = List.fold_left (^) "" lst |> Interp.parse |> translate_prog in
  print_string ("\n" ^ string ^ "\n")

(** [run_translate ()] asks the user to enter the name of a file, reads the 
    file and produces a [.ml] file containing the OCaml translation of the
    OMaskl code in that file. *)
let run_translate () = 
  print_string
    ("\nPlease enter the name of the .txt file you want to translate." ^  
     "\nDisclaimer: If the file you entered calls print or print_endl, " ^ 
     "there may be errors in the ml file.\n" ^ 
     "Please note the translator does not support translating classes and" ^ 
     " other Object-Oriented functionality of OMaskl.\n");
  ANSITerminal.(print_string [cyan] ">-> ");
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> let lst = file_name |> open_in |> read_file [] in
    let new_file_name = output lst file_name in 
    ANSITerminal.(
      print_string [green] ("The OCaml translation of your OMaskl code is in "
                            ^ new_file_name ^ "\n"))

(** [translate_interp ()] asks the user to enter the name of a file, 
    reads the file, and prints the OCaml translation of the OMaskl 
    code in that file. Exits if the user enters ["quit"]. *)
let rec translate_interp () = 
  print_string
    ("\nPlease enter the name of the .txt file you want to translate." ^ 
     "Please note the translator does not support translating classes and" ^ 
     " other Object-Oriented functionality of OMaskl.\n");
  ANSITerminal.(print_string [cyan] ">-> ");
  match read_line () with
  | exception End_of_file -> ()
  | "quit" -> ()
  | file_name -> let lst = file_name |> open_in |> read_file [] in
    interp_output lst;

    translate_interp ()

(** [read_input () lst] is the list where each element 
    is a line of input from the user. Stops reading input when
    the user input ends with [";_;"].  *)
let rec read_input () lst = 
  match read_line () with 
  | exception End_of_file -> lst 
  | e -> if String.length e >= 3 
    then let str = String.sub e (String.length e - 3) 3 in 
      if str = ";_;"
      then (lst @ [String.sub e 0 (String.length e - 3)])
      else read_input () (lst @ [e ^ " "])
    else read_input () (lst @ [e ^ " "])

(** [eval_interp env ()] interprets the user input and prints
    the result of evaluating the expression the user enters. 
    Terminates when the user enters ["#quit;_;"].  *)
let rec eval_interp env () =
  ANSITerminal.(print_string [cyan] "#_# ");
  try 
    let e = List.fold_left (^) "" (read_input () []) in
    if (String.trim e) = "#quit" then () else
      let value = Interp.interp env e in 
      begin match value with 
        | Environment env1 -> 
          print_string ((Eval.string_of_value value) ^ "\n\n"); 
          eval_interp env1 ()
        | _ -> print_string ((Eval.string_of_value value) ^ "\n\n"); 
          eval_interp env ()
      end
  with exc -> begin match exc with 
      | Failure f -> ANSITerminal.(
          print_string  [red] ("Invalid expression: " ^ f ^ "\n\n"))
      | _ -> ANSITerminal.(print_string  [red] ("Invalid expression\n\n"))
    end; eval_interp env ()

(** [run_code lst] evaluates the OMaskl code stored in [lst].  *)
let run_code lst = 
  List.fold_left (^) "" lst |> Interp.interp Eval.initialize_env

(** [evaluate ()] asks the user to input the name of a file to evaluate and
    evaluates that file.  *)
let evaluate () = 
  print_string
    "\nPlease enter the name of the .txt file you want to run.\n";
  ANSITerminal.(print_string [cyan] ">-> ");
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> let lst = file_name |> open_in |> read_file [] in
    ignore (run_code lst);
    ANSITerminal.(
      print_string [green] ("Your code has been run successfully." ^ "\n"))

(** [read_commands ()] is a REPL that asks for the user to input a command
    and runs that command.  *)
let rec read_commands () = 
  print_endline "Please enter a command.";
  ANSITerminal.(print_string [cyan] "> ");
  match read_line () with 
  | command -> begin match Command.parse command with 
      | Translate -> run_translate ()
      | TranslateInterp -> translate_interp ()
      | Eval -> evaluate ()
      | EvalInterp -> ANSITerminal.(
          print_string [green] ("\n\nWelcome to the OMaskl interpreter!\n"));
        eval_interp Eval.initialize_env ()
      | Commands -> ANSITerminal.(
          print_string [magenta]
            "translate\ntranslate interp\nevaluate\nevaluate interp\nquit\n")
      | Quit -> exit 0
      | Invalid ->  ANSITerminal.(
          print_string [red] 
            ("That is not a valid command. " 
             ^ "To see the commands, enter the command \"commands\"\n"))
    end; print_string "\n";
    read_commands ()

let main () =
  ANSITerminal.(print_string [green] "\n\nWelcome to OMaskl.\n");
  read_commands ()

(* Execute the program. *)
let () = main ()