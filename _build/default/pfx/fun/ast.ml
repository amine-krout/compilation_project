type command =
  (* DefineMe (* Question 4.1 *) *)
  | PUSH of int
  | ADD
  | SUB
  | MUL
  | DIV
  | REM
  | POP (* Remove element *)
  | SWAP (* Swap elements *)
  (* extending the Pfx langugae to support functions and applications *)  
  | Q of command list
  | EXEC
  | GET
	
type program = int * command list

(* add here all useful functions and types  related to the AST: for instance  string_of_ functions *)

let rec string_of_command = function
  | PUSH n -> "PUSH" ^ string_of_int n 
  | ADD -> "ADD"
  | SUB -> "SUB" 
  | MUL -> "MULT"
  | DIV -> "DIV"
  | REM -> "REM"
  | POP -> "POP"
  | SWAP -> "SWAP"
  (* extending the Pfx language to support functions and applications *)
  | Q of command list -> string_of_command Q 
  | EXEC -> "EXEC"
  | GET -> "GET"

let string_of_commands cmds = String.concat " " (List.map string_of_command cmds)

let string_of_program (args, cmds) = Printf.sprintf "%i args: %s\n" args (string_of_commands cmds)

