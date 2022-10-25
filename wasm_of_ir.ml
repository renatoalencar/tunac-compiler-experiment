
[@@@warning "-37-32"]

open IR
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
  let alloc_param params =
    match params with
    | [] -> Cconst_i32 0l
    | [ value ] -> value
    | _ -> assert false
  in

  match op, params with
  | Capply name, params -> Expression.Call.make wasm_mod name (List.map (compile_expression wasm_mod) params) Type.int32
  | Cload cell, [ ptr ] -> Expression.Load.make wasm_mod 4 (cell * 4) 0 Type.int32 (compile_expression wasm_mod ptr)
  | Calloc size, params ->
    Expression.Block.make wasm_mod (gensym "alloc")
      [ Expression.Global_set.make wasm_mod "heap_top"
          (Expression.Binary.make wasm_mod Op.add_int32
            (Expression.Global_get.make wasm_mod "heap_top" Type.int32)
            (Expression.Binary.make wasm_mod Op.add_int32
              (compile_expression wasm_mod (alloc_param params))
              (Expression.Const.make wasm_mod (Literal.int32 (Int32.of_int (size * 4))))))
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

  | Cwhile (condition, statement) ->
    let name = gensym "loop" in
    loop_stack := name :: !loop_stack;
    let loop =
      Expression.Loop.make wasm_mod name
        (Expression.If.make wasm_mod
          (compile_expression wasm_mod condition)
          (Expression.Block.make wasm_mod (gensym "while_body")
            [ compile_statement wasm_mod statement
            ; Expression.Break.make wasm_mod name
                (Expression.Null.make ())
                (Expression.Null.make ()) ])
          (Expression.Null.make ()))
    in
    loop_stack := List.tl !loop_stack;
    loop

  | Ccontinue ->
    (* WASM break on loops works more like a continue than a break *)
    Expression.Break.make wasm_mod (List.hd !loop_stack) (Expression.Null.make ()) (Expression.Null.make ())

let compile_ir ~env ast =
  let wasm_mod = Module.create () in
  
  let locals = Array.make (IR_of_michelson.Env.max env + 1) Type.int32 in
  let expr = compile_statement wasm_mod ast in
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

  Import.add_function_import wasm_mod "parameter_size" "env" "parameter_size" Type.none Type.int32;
  Import.add_function_import wasm_mod "parameter_load" "env" "parameter_load" Type.int32 Type.int32;

  Memory.set_memory wasm_mod 1 10 "memory" [] true;

  ignore @@ Module.validate wasm_mod;

  wasm_mod