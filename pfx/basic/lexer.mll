{
  open Parser
  
  (* type token = 
  | EOF | ADD | SUB | MUL | DIV | REM | LPAR | RPAR 
  | INT of int  *)

  let print_token = function 
  | EOF   -> print_string "EOF " 
  | ADD   -> print_string "ADD "
  | SUB   -> print_string "SUB "
  | MUL   -> print_string "MUL "
  | DIV   -> print_string "DIV "
  | REM   -> print_string "REM "
  | POP   -> print_string "POP "
  | SWAP  -> print_string "SWAP "
  | PUSH  -> print_string "PUSH "
  | INT n -> print_int n ; print_string " " 

  let mk_int nb =
    try INT (int_of_string nb)
    with Failure _ -> failwith (Printf.sprintf "Illegal integer '%s': " nb)
}

let newline = (['\n' '\r'] | "\r\n")
let blank = [' ' '\014' '\t' '\012']
let not_newline_char = [^ '\n' '\r']
let digit = ['0'-'9']

rule token = parse
  (* newlines *)
  | newline { token lexbuf }
  (* blanks *)
  | blank + { token lexbuf }
  (* end of file *)
  | eof      { EOF }
  (* comments *)
  | "--" not_newline_char*  { token lexbuf }
  (* integers *)
  | digit+ as nb           { mk_int nb }
  (* commands  *)
  (***** TO COMPLETE *****)
  | "add"  {ADD}
  | "sub"  {SUB}
  | "mul"  {MUL}
  | "div"  {DIV}
  | "rem"  {REM}
  | "pop"  {POP}
  | "swap" {SWAP}
  | "push" {PUSH}

  (* illegal characters *)
  | _ as c                  { failwith (Printf.sprintf "Illegal character '%c': " c) }

{
  
  (* Main function commented so the program uses directly the function in the pfxVM.ml *)
  
  (* let rec examine_all lexbuf = 
    let result = token lexbuf in 
    print_token result;
    print_string "";
    match result with
    | EOF -> ()
    | _ -> examine_all lexbuf

  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error _ ->
    print_endline ("Can't find file '" ^ file ^ "'")
  let _ = Arg.parse [] compile "" *)

}