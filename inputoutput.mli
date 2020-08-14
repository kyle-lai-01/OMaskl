(** Handles input and output for the system.  *)
open Ast

(** [read_file lst channel] is the list of strings containing
    the code in our language read from the file through in_channel [channel].
    [lst] is an accumulator list. *)
val read_file : string list -> in_channel -> string list

(** [add_to_lst lst channel str] is the string [str ^ " "] appended
    to [lst] and the list read from the file through in_channel [channel]*)
val add_to_lst : string list -> in_channel -> string -> string list


(** [translate_prog p] is the [string] of [OCaml] code
    represented by expression [p]. *)
val translate_prog : Ast.expr -> string

(** [output lst file_name] is the name of the file in which the
    string of [OCaml] code translated is from the code in our language stored 
    in [lst] in file with 
    [file_name] except [_output.ml] instead of [.txt] *)
val output : string list -> string -> string

(** [main ()] prompts for the file to translate, then translates it. *)
val main : unit -> unit