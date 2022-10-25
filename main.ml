
[@@@warning "-37-32"]

open Tezos_micheline

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

open Binaryen

let _ =
  let c, env = IR_of_michelson.compile_contract code in
  let wasm_mod = Wasm_of_ir.compile_ir ~env c in

  Module.print wasm_mod;

  let output = open_out_bin "mod.wasm" in
  let mod_, _ = Module.write wasm_mod None in
  (* output_bytes stdout mod_; *)
  output_bytes output mod_;
  close_out output