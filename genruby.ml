open Ast
open Type
open Common

module Transformer = struct
  type adjusted_expr = {
    a_expr : texpr;
    a_blocks : texpr list;
    a_next_id : unit -> string;
    a_is_value : bool;
  }

  let new_counter () =
    let n = ref (-1) in
    (fun () ->
      incr n;
      Printf.sprintf "_hx_local_%i" !n
    )

  let lift_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) e =
    let next_id = match next_id with
      | None ->
        new_counter()
      | Some f ->
        f
    in
    {
      a_expr = e;
      a_blocks = blocks;
      a_next_id = next_id;
      a_is_value = is_value
    }

  let lift_expr1 is_value next_id blocks e =
    lift_expr ~is_value:is_value ~next_id:(Some next_id) ~blocks:blocks e

  let rec transform_expr ?(is_value = false) ?(next_id = None) ?(blocks = []) (e: texpr) : adjusted_expr =
    transform1 (lift_expr ~is_value ~next_id ~blocks e)
  and transform_expr1 is_value next_id blocks e =
    transform_expr ~is_value ~next_id:(Some next_id) ~blocks e
  and transform1 ae : adjusted_expr =
    let trans is_value blocks e = transform_expr1 is_value ae.a_next_id blocks e in
    let lift is_value blocks e = lift_expr1 is_value ae.a_next_id blocks e in
    let a_expr = ae.a_expr in
    match ae.a_is_value,ae.a_expr.eexpr with

  let to_expr ae = ae.a_expr

  let transform_to_value e =
    to_expr (transform1 (lift_expr e ~is_value:true))
end

module Printer = struct
  type print_context = {
    pc_indent : string;
    pc_next_anon_func : unit -> string;
    pc_debug : bool;
  }

  let create_context =
    let n = ref (-1) in
    (fun indent debug -> {
      pc_indent = indent;
      pc_next_anon_func = (fun () -> incr n; Printf.sprintf "anon_%i" !n);
      pc_debug = debug;
    }
    )
end

module Generator = struct
  type context = {
    com : Common.context;
    buf : Buffer.t;
  }

	let transform_to_value e =
		Transformer.transform_to_value e

  let mk_context com = {
    com = com;
    buf = Buffer.create 16000;
  }

  let spr ctx s =
    Buffer.add_string ctx.buf s

  let print ctx =
    Printf.kprintf (fun s -> begin
      Buffer.add_string ctx.buf s
    end)

  let newline ctx =
    spr ctx "\n"

  let gen_expr ctx e field indent =
    let pctx = Printer.create_context ("\t" ^ indent) ctx.com.debug in
    let e = match e.eexpr with
      | TFunction(f) ->
          {e with eexpr = TBlock [e]}
      | _ ->
          e
    in
    let expr2 = transform_to_value e in
    let name = "_hx_init_" ^ (String.concat "_" (ExtString.String.nsplit field ".")) in
    spr ctx name


  let gen_main ctx =
    match ctx.com.main with
      | None ->
        ()
      | Some e ->
        newline ctx;
        gen_expr ctx e "" ""

  let run com =
    let ctx = mk_context com in
    gen_main ctx;
    Buffer.add_string ctx.buf "# This is a test.\n";
    mkdir_from_path com.file;
    let ch = open_out_bin com.file in
    output_string ch (Buffer.contents ctx.buf);
    close_out ch
end

let generate com =
  Generator.run com
