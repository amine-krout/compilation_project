
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SWAP
    | SUB
    | REM
    | PUSH
    | POP
    | MUL
    | INT of (
# 12 "pfx/basic/parser.mly"
       (int)
# 21 "pfx/basic/parser.ml"
  )
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

# 1 "pfx/basic/parser.mly"
  
  (* Ocaml code here*)
  open Ast

# 36 "pfx/basic/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState01 : ('s _menhir_cell0_INT, _menhir_box_program) _menhir_state
    (** State 01.
        Stack shape : INT.
        Start symbol: program. *)

  | MenhirState13 : (('s, _menhir_box_program) _menhir_cell1_instruction, _menhir_box_program) _menhir_state
    (** State 13.
        Stack shape : instruction.
        Start symbol: program. *)


and ('s, 'r) _menhir_cell1_instruction = 
  | MenhirCell1_instruction of 's * ('s, 'r) _menhir_state * (Ast.command)

and 's _menhir_cell0_INT = 
  | MenhirCell0_INT of 's * (
# 12 "pfx/basic/parser.mly"
       (int)
# 57 "pfx/basic/parser.ml"
)

and _menhir_box_program = 
  | MenhirBox_program of (Ast.program) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 36 "pfx/basic/parser.mly"
               ( ADD )
# 68 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_02 =
  fun () ->
    (
# 37 "pfx/basic/parser.mly"
               ( SUB )
# 76 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_03 =
  fun () ->
    (
# 38 "pfx/basic/parser.mly"
               ( MUL )
# 84 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_04 =
  fun () ->
    (
# 39 "pfx/basic/parser.mly"
               ( DIV )
# 92 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_05 =
  fun () ->
    (
# 40 "pfx/basic/parser.mly"
               ( REM )
# 100 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_06 =
  fun () ->
    (
# 41 "pfx/basic/parser.mly"
               ( POP )
# 108 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_07 =
  fun () ->
    (
# 42 "pfx/basic/parser.mly"
               ( SWAP )
# 116 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_08 =
  fun n ->
    (
# 43 "pfx/basic/parser.mly"
               ( PUSH n )
# 124 "pfx/basic/parser.ml"
     : (Ast.command))

let _menhir_action_09 =
  fun () ->
    (
# 32 "pfx/basic/parser.mly"
    ( [] )
# 132 "pfx/basic/parser.ml"
     : (Ast.command list))

let _menhir_action_10 =
  fun instr instrs ->
    (
# 33 "pfx/basic/parser.mly"
                                             ( instr :: instrs )
# 140 "pfx/basic/parser.ml"
     : (Ast.command list))

let _menhir_action_11 =
  fun i q ->
    (
# 29 "pfx/basic/parser.mly"
                                       ( i, q )
# 148 "pfx/basic/parser.ml"
     : (Ast.program))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | DIV ->
        "DIV"
    | EOF ->
        "EOF"
    | INT _ ->
        "INT"
    | MUL ->
        "MUL"
    | POP ->
        "POP"
    | PUSH ->
        "PUSH"
    | REM ->
        "REM"
    | SUB ->
        "SUB"
    | SWAP ->
        "SWAP"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_11 : type  ttv_stack. ttv_stack _menhir_cell0_INT -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell0_INT (_menhir_stack, i) = _menhir_stack in
      let q = _v in
      let _v = _menhir_action_11 i q in
      MenhirBox_program _v
  
  let rec _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_program) _menhir_cell1_instruction -> _ -> _menhir_box_program =
    fun _menhir_stack _v ->
      let MenhirCell1_instruction (_menhir_stack, _menhir_s, instr) = _menhir_stack in
      let instrs = _v in
      let _v = _menhir_action_10 instr instrs in
      _menhir_goto_instruction_seq _menhir_stack _v _menhir_s
  
  and _menhir_goto_instruction_seq : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _v
      | MenhirState01 ->
          _menhir_run_11 _menhir_stack _v
  
  let rec _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instruction (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SWAP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_07 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | SUB ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_02 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | REM ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_05 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | PUSH ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState13
      | POP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_06 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | MUL ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_03 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | DIV ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_04 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | ADD ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_01 () in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState13 _tok
      | EOF ->
          let _v = _menhir_action_09 () in
          _menhir_run_14 _menhir_stack _v
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_program) _menhir_state -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let n = _v in
          let _v = _menhir_action_08 n in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let rec _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_program =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          let _menhir_stack = MenhirCell0_INT (_menhir_stack, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | SWAP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_07 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | SUB ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_02 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | REM ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_05 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | PUSH ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState01
          | POP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_06 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | MUL ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_03 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | DIV ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_04 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | ADD ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_01 () in
              _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState01 _tok
          | EOF ->
              let _v = _menhir_action_09 () in
              _menhir_run_11 _menhir_stack _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let program =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_program v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 45 "pfx/basic/parser.mly"
  

# 315 "pfx/basic/parser.ml"
