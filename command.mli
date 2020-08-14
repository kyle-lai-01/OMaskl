(** The commands used for the outer REPL. *)

(** The type of commands for the main repl.
    [Translate] is the command to translate OMaskl code to OCaml
    and produce a .ml file. 
    [TranslateInterp] is the command to translate OMaskl code to OCaml
    and print the result. 
    [Eval] is the command to run the OMaskl code in a file. 
    [EvalInterp] is the command to run the interpreter. 
    [Commands] is the command to see all available commands. 
    [Quit] is the command to quit. 
    [Invalid] is an invalid command. *)
type command = 
  | Translate | TranslateInterp | Eval | EvalInterp | Commands | Quit | Invalid

(** [parse s] is the command represented by string [s]. *)
val parse : string -> command