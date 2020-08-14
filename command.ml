type command = 
  | Translate | TranslateInterp | Eval | EvalInterp | Commands | Quit | Invalid

let parse string = match string with
  | "translate" -> Translate
  | "translate interp" -> TranslateInterp
  | "evaluate" -> Eval
  | "evaluate interp" -> EvalInterp
  | "commands" -> Commands
  | "quit" -> Quit
  | _ -> Invalid