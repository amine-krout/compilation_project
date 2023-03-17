(* The type of the commands for the stack machine *)
type command =
| PUSH of int
| ADD
| SUB
| MUL
| DIV
| REM
| POP (* Remove element *)
| SWAP (* Swap elements *)

(* The type for programs *)
type program = int * command list

(* Converting a command to a string for printing *)
val string_of_command : command -> string

(* Converting a program to a string for printing *)
val string_of_program : program -> string
