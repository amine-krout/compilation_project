open Ast
(* Question 5.2*)
(* let generate = function
  | Const _ -> failwith "To implement"
  | Binop(_,_,_) -> failwith "To implement"
  | Uminus _ -> failwith "To implement"
  | Var _ -> failwith "Not yet supported" *)

  let rec generate (e : Ast.expression) (env : (string * int) list) : BasicPfx.Ast.command list = 
    match e with 
    | Const n -> [PUSH n] 
    | Binop(op,e1,e2) -> 
      generate e2 env @
      generate e1 env @
      (match op with 
       | BinOp.Badd -> [ADD]
       | BinOp.Bsub -> [SUB]
       | BinOp.Bmul -> [MUL]
       | BinOp.Bdiv -> [DIV]
       | BinOp.Bmod -> [REM]
      )
    | Uminus expr -> generate expr env @ generate (Const 0) env @ [SUB]
    | Var x -> [GET (List.assoc x env)]
    (* Questions 10.3 + 11.2 : the following part is a bit complicated *)
    | Lambda (x, body) -> 
      let env' = (x, List.length env) :: env in
      let body_code = generate body env' in
      [EXEC_SEQ; SEQ_START] @ body_code @ [SEQ_END; PUSH (List.length env); SWAP; POP] 
    | App (e1, e2) ->
      let arg_code = generate e2 env in
      let fun_code = generate e1 env in
      fun_code @ [EXEC; PUSH (List.length env + 1)] @ arg_code @ [SWAP; SEQ_START] @
      (List.init (List.length env) (fun i -> [GET (i + 1); SWAP; GET i; SWAP])) |> List.concat @
      [SEQ_END; PUSH (List.length env + 1); SWAP; POP] 
    | Let (x, e1, e2) ->
      let env' = (x, 0) :: List.map (fun (y, n) -> (y, n + 1)) env in
      generate (Lam (x, e2)) env' ^ " " ^ generate e1 env ^ " exec"
  