open Ast
open Printf

let string_of_stack stack = sprintf "[%s]" (String.concat ";" (List.map string_of_int stack))

let string_of_state (cmds,stack) =
  (match cmds with
   | [] -> "no command"
   | cmd::_ -> sprintf "executing %s" (string_of_command cmd))^
    (sprintf " with stack %s" (string_of_stack stack))

(* Question 4.2 *)
let step state =
  match state with
  | [], _ -> Error("Nothing to step",state)
  (* Valid configurations *)
  
  | PUSH n :: q , stack -> Ok (q, n::stack)
  
  | ADD :: q, stack ->
  ( match stack with 
    | x :: y :: s -> Ok (q, (x+y) :: s)
    | _ -> Error("Not enough arguments for ADD", ([], stack))
  )
  
  | SUB :: q, stack ->
  ( match stack with 
    | x :: y :: s -> Ok (q, (x-y) :: s)
    | _ -> Error("Not enough arguments for SUB", ([], stack))
  ) 
  
  | MUL :: q, stack -> 
  ( match stack with 
    | x :: y :: s -> Ok (q, (x*y) :: s)
    | _ -> Error("Not enough arguments for MUL", ([], stack))
  )
  
  | DIV :: q, stack ->
  ( match stack with 
    | x :: y :: s -> Ok (q, (x/y) :: s)
    | _ -> Error("Not enough arguments for DIV", ([], stack))
  )  
  
  | REM :: q, stack -> 
  ( match stack with
    | x :: y :: s -> Ok (q, (x mod y) :: s)
    | _ -> Error("Not Enough arguments for REM", ([], stack))
  )

  | POP :: q, stack -> 
  ( match stack with 
    | _ :: s -> Ok (q, s)
    | _ -> Error("Empty stack", ([], stack))
  ) 
  
  | SWAP :: q, stack -> 
  ( match stack with 
    | x :: y :: s -> Ok (q, y :: x :: s)
    | _ -> Error("Not enough arguments for SWAP", ([], stack))
  )

  (* To be able to push an executable sequence into the stack when incountring the SEQ_START and the SEQ_END *)

  | SEQ_START :: q, stack ->
    Ok (q, (Q[]) :: stack)

  | SEQ_END :: q, stack ->
  ( match stack with 
    | (Q cmds) :: s -> Ok (q, (List.rev cmds) @ s)
    | _ -> Error ("SEQ_END without SEQ_START", ([], stack))
  )

  | Q cmds :: q, stack -> Ok (q, (Q (List.rev cmds)) :: stack)
  
  (* | DefineMe :: q , stack          -> Ok (q, stack) *)

let eval_program (numargs, cmds) args =
  let rec execute = function
    | [], []    -> Ok None
    | [], v::_  -> Ok (Some v)
    | state ->
       begin
         match step state with
         | Ok s    -> execute s
         | Error e -> Error e
       end
  in
  if numargs = List.length args then
    match execute (cmds,args) with
    | Ok None -> printf "No result\n"
    | Ok(Some result) -> printf "= %i\n" result
    | Error(msg,s) -> printf "Raised error %s in state %s\n" msg (string_of_state s)
  else printf "Raised error \nMismatch between expected and actual number of args\n"
