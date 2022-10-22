
[@@@warning "-37-32"]

open Tezos_micheline
open Micheline
open Michelson_v1_primitives

(* 
let code =
  [ Prim (I_CDR, 0, [], [])
  ; Prim (I_NIL, 0, [ Prim (T_operation, 0, [], []) ], [])
  ; Prim (I_PAIR, 0, [], []) ] *)

  
type var = int
[@@deriving show]
type global = string
[@@deriving show]

module Env = struct
  module Set = Set.Make(Int)

  type t = { mutable allocated: Set.t; mutable max: int }

  let make () = { allocated = Set.empty; max = -1 }

  let max t = t.max

  let alloc_local t =
    let rec aux reg =
      if Set.mem reg t.allocated then
        aux (reg + 1)
      else if reg > t.max then (
        t.max <- reg;
        t.allocated <- Set.add reg t.allocated;
        reg
      ) else (
        t.allocated <- Set.add reg t.allocated;
        reg
      )
    in
    aux 0

  let free_local t local =
    t.allocated <- Set.remove local t.allocated

end

type wasm_operation =
  | Wasm_clz
  | Wasm_ctz
  | Wasm_popcnt
  | Wasm_add
  | Wasm_sub
  | Wasm_mul
  | Wasm_div
  | Wasm_rem
  | Wasm_and
  | Wasm_or
  | Wasm_xor
  | Wasm_shl
  | Wasm_shr
  | Wasm_rotl
  | Wasm_rotr
  | Wasm_eqz
  | Wasm_eq
  | Wasm_ne
  | Wasm_lt
  | Wasm_gt
  | Wasm_le
  | Wasm_ge
[@@deriving show]

type operation =
  | Capply of string
  | Cload of int
  | Calloc of int
  | Cwasm of wasm_operation
[@@deriving show]

type expression =
  | Cconst_i32 of int32
  | Cvar of var
  | Cglobal of global
  | Cop of operation * expression list
  [@@deriving show]
  
type statement =
  | Cassign of var * expression
  | Cglobal_assign of global * expression
  | Cifthenelse of expression * statement * statement
  | Cloop of statement
  | Cbreak
  | Cblock of statement list
  | Cstore of int * expression * expression
[@@deriving show]

let list_cons var hd tl =
  Cblock
   [ Cassign (var, Cop (Calloc 2, []))
   ; Cstore (0, Cvar var, hd)
   ; Cstore (1, Cvar var, tl) ]

let compile_car expr = Cop (Cload 0, [ expr ])

let compile_cdr expr = Cop (Cload 1, [ expr ])

let compile_pop var =
  Cblock
    [ Cassign (var, compile_car (Cglobal "stack"))
    ; Cglobal_assign ("stack", compile_cdr (Cglobal "stack")) ]

let compile_push ~env expr =
  let cell = Env.alloc_local env in
  let block =
    Cblock
      [ list_cons cell expr (Cglobal "stack")
      ; Cglobal_assign ("stack", Cvar cell) ]
  in
  Env.free_local env cell;
  block

let compile_pair ~env =
  let cell = Env.alloc_local env in
  let item = Env.alloc_local env in
  let block =
    Cblock
      [ Cassign (cell, Cop (Calloc 2, []))
      ; compile_pop item
      ; Cstore (0, Cvar cell, Cvar item)
      ; compile_pop item
      ; Cstore (1, Cvar cell, Cvar item)
      ; compile_push ~env (Cvar cell) ]
  in
  Env.free_local env cell;
  Env.free_local env item;
  block

let compile_dig ~env n =
  let n = Int32.sub n 1l in
  let counter =  Env.alloc_local env in
  let node = Env.alloc_local env in
  let loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cloop
          (Cblock
            [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
            ; Cassign (node, compile_cdr (Cvar node))
            ; Cifthenelse (Cvar counter, Cbreak, Cblock []) ]) ]
  in
  Env.free_local env counter;
  let a = Env.alloc_local env in
  let block =
    Cblock
      [ loop
      ; Cassign (a, compile_cdr (Cvar node))
      ; Cstore (1, Cvar node, compile_cdr (Cvar a))
      ; Cstore (1, Cvar a, Cglobal "stack")
      ; Cglobal_assign ("stack", Cvar a) ]
  in
  Env.free_local env a;
  Env.free_local env node;
  block

let compile_dug ~env n =
  let n = Int32.sub n 1l in
  let node = Env.alloc_local env in
  let counter = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, compile_cdr (Cglobal "stack"))
      ; Cloop
          (Cblock
            [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
            ; Cassign (node, compile_cdr (Cvar node))
            ; Cifthenelse (Cvar counter, Cbreak, Cblock [])]) ]
  in
  Env.free_local env counter;
  let head = Env.alloc_local env in
  let block =
    Cblock
      [ inner_loop
      ; Cassign (head, Cglobal "stack")
      ; Cglobal_assign ("stack", compile_cdr (Cvar head))
      ; Cstore (1, Cvar head, compile_cdr (Cvar node))
      ; Cstore (1, Cvar node, Cvar head) ]
  in
  Env.free_local env node;
  Env.free_local env head;
  block

let compile_drop ~env n =
  let counter = Env.alloc_local env in
  let node = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cloop
         (Cifthenelse (
            Cvar counter
            , Cblock
                [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
                ; Cassign (node, compile_cdr (Cvar node))
                ; Cbreak ]
            , Cblock [])) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; Cglobal_assign ("stack", Cvar node) ]
  in
  Env.free_local env node;
  block

let compile_dup ~env n =
  let n = Int32.sub n 1l in
  let counter = Env.alloc_local env in
  let node = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cloop
          (Cifthenelse
            (Cvar counter
            , Cblock
                [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
                ; Cassign (node, compile_cdr (Cvar node))
                ; Cbreak ]
            , Cblock [])) ]
  in
  Env.free_local env counter;
  let block =
    Cblock
      [ inner_loop
      ; compile_push ~env (compile_car (Cvar node)) ]
  in
  Env.free_local env node;
  block

let compile_dip ~env n block =
  let n = Int32.sub n 1l in
  let node = Env.alloc_local env in
  let counter = Env.alloc_local env in
  let inner_loop =
    Cblock
      [ Cassign (counter, Cconst_i32 n)
      ; Cassign (node, Cglobal "stack")
      ; Cloop
          (Cifthenelse (
            Cvar counter
            , Cblock
                [ Cassign (counter, Cop (Cwasm Wasm_sub, [ Cvar counter; Cconst_i32 1l ]))
                ; Cassign (node, compile_cdr (Cvar node)) ]
            , Cblock [] )) ]
  in
  Env.free_local env counter;

  let pair = Env.alloc_local env in
  let save_stack_block =
    Cblock
      [ Cassign (pair, Cop (Calloc 2, []))
      ; Cstore (0, Cvar pair, Cglobal "stack")
      ; Cstore (1, Cvar pair, Cvar node)
      ; Cglobal_assign ("dip_stack", Cop (Cwasm Wasm_add, [ Cglobal "dip_stack"; Cconst_i32 4l ]))
      ; Cstore (0, Cglobal "dip_stack", Cvar pair)
      ; Cglobal_assign ("stack", compile_cdr (Cvar node)) ]
  in
  Env.free_local env pair;
  Env.free_local env node;

  (* Deallocate and allocate again so it does not conflict with DIP's internal block *)
  let pair = Env.alloc_local env in
  let restore_stack =
    Cblock
      [ Cassign (pair, Cop (Cload 0, [ Cglobal "dip_stack" ]))
      ; Cstore (1, compile_cdr (Cvar pair), Cglobal "stack")
      ; Cglobal_assign ("stack", compile_car (Cvar pair)) 
      ; Cglobal_assign ("dip_stack", Cop (Cwasm Wasm_sub, [ Cglobal "dip_stack"; Cconst_i32 4l ] )) ]
  in

  Cblock [ inner_loop; save_stack_block; block; restore_stack ]

let rec compile_instruction ~env instr =
  match instr with
  | Prim (_, I_CAR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_car (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_CDR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_cdr (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_UNPAIR, _, _) ->
    let top = Env.alloc_local env in
    let block =
      Cblock [ compile_pop top
             ; compile_push ~env (compile_cdr (Cvar top))
             ; compile_push ~env (compile_car (Cvar top)) ]
    in
    Env.free_local env top;
    block

  | Prim (_, I_ADD, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_pop y
             ; compile_push ~env (Cop (Cwasm Wasm_add, [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    block

  | Prim (_, I_SUB, _, _) ->
    let x = Env.alloc_local env in
    let y = Env.alloc_local env in
    let block =
      Cblock [ compile_pop x
             ; compile_pop y
             ; compile_push ~env (Cop (Cwasm Wasm_sub, [ Cvar x; Cvar y ])) ]
    in
    Env.free_local env x;
    Env.free_local env y;
    block
  

  | Prim (_, I_NIL, _, _) ->
    compile_push ~env (Cconst_i32 0l)

  | Prim (_, I_PAIR, _, _) ->
    compile_pair ~env

  | Prim (_, I_IF_LEFT, [ Seq (_, left_branch); Seq (_, right_branch) ], _) ->
    let p = Env.alloc_local env in
    let block =
      Cblock [ compile_pop p
             ; compile_push ~env (Cop (Cload 1, [ Cvar p ]))
             ; Cifthenelse
                (Cop (Cload 0, [ Cvar p ])
                , Cblock (List.map (compile_instruction ~env) left_branch)
                , Cblock (List.map (compile_instruction ~env) right_branch)) ]
    in
    Env.free_local env p;
    block

  | Prim (_, I_SWAP, _, _) ->
    let fst = Env.alloc_local env in
    let snd = Env.alloc_local env in
    let block =
      Cblock [ compile_pop fst
             ; compile_pop snd
             ; compile_push ~env (Cvar fst)
             ; compile_push ~env (Cvar snd) ]
    in
    Env.free_local env fst;
    Env.free_local env snd;
    block

  | Prim (_, I_PUSH, [ Prim (_, T_int, _, _); Int (_, z) ], _) ->
    let value = Z.to_int32 z in
    compile_push ~env (Cconst_i32 value)

  | Prim (_, I_DIG, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_dig ~env n

  | Prim (_, I_DUG, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_dug ~env n

  | Prim (_, I_DROP, [], _) ->
    compile_drop ~env 1l

  | Prim (_, I_DROP, [ Int (_, n) ], _) ->
    let n = Z.to_int32 n in
    compile_drop ~env n

  | Prim (_, I_DUP, [], _) ->
    compile_dup ~env 1l

  | Prim (_, I_DUP, [ Int (_, n) ], _) ->
    compile_dup ~env (Z.to_int32 n)

  | Prim (_, I_DIP, [ Int (_, n); Seq (_, instr) ], _) ->
    let n = Z.to_int32 n in
    let block = Cblock (List.map (compile_instruction ~env) instr) in
    if n = 0l then block
    else compile_dip ~env n block

  | Prim (_, I_DIP, [ Seq (_, instr) ], _) ->
    compile_dip ~env 1l (Cblock (List.map (compile_instruction ~env) instr))

  | _ -> assert false


open Binaryen

let gensym_count = ref 0
let gensym name =
  incr gensym_count;
  Printf.sprintf "%s.%d" name !gensym_count

let rec compile_expression wasm_mod expr =
  match expr with
  | Cglobal global -> Expression.Global_get.make wasm_mod global Type.int32
  | Cvar var -> Expression.Local_get.make wasm_mod var Type.int32
  | Cconst_i32 value -> Expression.Const.make wasm_mod (Literal.int32 value)
  | Cop (op, params) -> compile_operation wasm_mod op params

and compile_operation wasm_mod op params =
  match op, params with
  | Capply name, params -> Expression.Call.make wasm_mod name (List.map (compile_expression wasm_mod) params) Type.int32
  | Cload cell, [ ptr ] -> Expression.Load.make wasm_mod 4 (cell * 4) 0 Type.int32 (compile_expression wasm_mod ptr)
  | Calloc size, [] ->
    Expression.Block.make wasm_mod (gensym "alloc")
      [ Expression.Global_set.make wasm_mod "heap_top"
          (Expression.Binary.make wasm_mod Op.add_int32
            (Expression.Global_get.make wasm_mod "heap_top" Type.int32)
            (Expression.Const.make wasm_mod (Literal.int32 (Int32.of_int (size * 4)))))
      ; Expression.Global_get.make wasm_mod "heap_top" Type.int32 ]
  | Cwasm wasm_operation, params -> compile_wasm_operation wasm_mod wasm_operation params

  | _ -> assert false

and compile_wasm_operation wasm_mod operation params =
  match operation, params with
  | Wasm_add, [ a; b ] ->
    Expression.Binary.make wasm_mod Op.add_int32
      (compile_expression wasm_mod a)
      (compile_expression wasm_mod b)

  | Wasm_sub, [ a; b ] ->
    Expression.Binary.make wasm_mod Op.sub_int32
      (compile_expression wasm_mod a)
      (compile_expression wasm_mod b)

  | _ -> assert false

let loop_stack = ref []

let rec compile_statement wasm_mod statement =
  match statement with
  | Cblock statements ->
    Expression.Block.make wasm_mod (gensym "block")
      (List.map (compile_statement wasm_mod) statements)

  | Cassign (var, expr) ->
      Expression.Local_set.make wasm_mod var (compile_expression wasm_mod expr)

  | Cstore (cell, ptr, value) ->
    Expression.Store.make wasm_mod 4 (cell * 4) 0
      (compile_expression wasm_mod ptr)
      (compile_expression wasm_mod value)
      Type.int32

  | Cglobal_assign (global, value) ->
    Expression.Global_set.make wasm_mod global
      (compile_expression wasm_mod value)

  | Cifthenelse (condition, _if, _else) ->
    Expression.If.make wasm_mod
      (compile_expression wasm_mod condition)
      (compile_statement wasm_mod _if)
      (compile_statement wasm_mod _else)

  | Cloop statement ->
    let name = gensym "loop" in
    loop_stack := name :: !loop_stack;
    let loop =
      Expression.Loop.make wasm_mod name
        (compile_statement wasm_mod statement)
    in
    loop_stack := List.tl !loop_stack;
    loop

  | Cbreak ->
    Expression.Break.make wasm_mod (List.hd !loop_stack) (Expression.Null.make ()) (Expression.Null.make ())

let read_all () =
  let rec aux s =
    try
      aux (input_line stdin :: s)
    with End_of_file -> s
  in
  aux []
  |> List.rev
  |> String.concat "\n"

let code =
  let code = read_all () in
  let tokens, _ = Micheline_parser.tokenize code in
  let code, _ = Micheline_parser.parse_expression tokens in
  code
  |> Micheline.strip_locations
  |> Micheline.map (fun prim -> Michelson_v1_primitives.prim_of_string prim |> Result.get_ok)
  |> Micheline.root

let code =
  match code with
  | Seq (_, [ Prim (_, K_parameter, _, _); Prim (_, K_storage, _, _); Prim (_, K_code, [ Seq (_, code) ], _) ]) -> code
  | _ -> assert false

let _ =
  let env = Env.make () in
  let c = Cblock (List.map (compile_instruction ~env) code) in
  (* Format.printf "%a\n" pp_statement c; *)

  let wasm_mod = Module.create () in
  
  let locals = Array.make (Env.max env + 1) Type.int32 in
  let expr = compile_statement wasm_mod c in
  ignore @@ Function.add_function wasm_mod "main" Type.none Type.none locals expr;
  ignore @@ Export.add_function_export wasm_mod "main" "main";

  ignore @@
    Global.add_global wasm_mod "stack" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 0l));
  ignore @@ Export.add_global_export wasm_mod "stack" "stack";

  ignore @@
    Global.add_global wasm_mod "heap_top" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 512l));
  ignore @@ Export.add_global_export wasm_mod "heap_top" "heap_top";

  ignore @@
    Global.add_global wasm_mod "dip_stack" Type.int32 true
      (Expression.Const.make wasm_mod (Literal.int32 256l));

  Memory.set_memory wasm_mod 1 10 "memory" [] true;

  ignore @@ Module.validate wasm_mod;

  (* Module.print wasm_mod; *)

  let output = open_out_bin "mod.wasm" in
  let mod_, _ = Module.write wasm_mod None in
  (* output_bytes stdout mod_; *)
  output_bytes output mod_;
  close_out output