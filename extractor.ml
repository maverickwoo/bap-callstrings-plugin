open Core_kernel.Std
open Bap.Std
open Or_error
open Program_visitor

let insert_functions t db =
  Sqlite3EZ.exec db
    "
-- we prefer to keep the target column in csk as a symbol, so this table is not really needed
create table functions (
  function_id integer primary key asc,
  symbol      text,
  begin_addr  integer,
  end_addr    integer
);
-- think of this as the edge of a graph
create table calls (
  call_id   integer primary key asc,
  from_func text,
  from_addr integer,
  to_func   text,
  to_addr   integer
);
-- think of this as the edge of a graph after scc contraction
create table calls_scc (
  call_id   integer primary key asc,
  from_func text,
  from_addr integer,
  to_func   text
);
-- we use a view, but this can also be a table or even a materialized view
create view cs1 as
  select calls.from_func as head, calls.from_func||\":\"||calls.from_addr as cs,
  calls.to_func as target, calls.to_addr as target_addr from calls;
";
  Table.iteri t.symbols ~f:begin fun smem src ->
    (* functions table *)
    let smin_addr = Memory.min_addr smem |> Word.to_int64 |> ok_exn in
    let smax_addr = Memory.max_addr smem |> Word.to_int64 |> ok_exn in
    (* we pick int64 to make the analysis applicable to more ISAs *)
    let query = sprintf "insert into functions values (null, \"%s\", %Ld, %Ld)"
        src smin_addr smax_addr
    in
    (* eprintf "%s\n" query; *)
    Sqlite3EZ.exec db query;

    (* calls table *)
    Disasm.insns_at_mem t.program smem |>
    Seq.iter ~f:begin fun (imem, insn) ->
      Bil.iter (object inherit [unit] Bil.visitor
        method! enter_int addr () =
          if in_jmp then
            let from_addr = Memory.min_addr imem |> Word.to_int64 |> ok_exn in
            (* bug: sometimes the number is small *)
            let _to_addr = addr |> Word.to_int64 |> ok_exn in
            (* so we check if addr is in sym table *)
            match Table.find_addr t.symbols addr with
              | None -> ()
              | Some (mem, dst) ->
                if Addr.(Memory.min_addr mem = addr) then
                  let to_addr = addr |> Word.to_int64 |> ok_exn in
                  let query2 =
                    sprintf "insert into calls values (null, %S, %Ld, %S, %Ld)"
                      src from_addr dst to_addr
                  in
                  (* eprintf "%s\n" query2; *)
                  Sqlite3EZ.exec db query2
      end) (Insn.bil insn)
    end
  end

(* directly define cs(k) (we could also define cs2, cs3, ..., csk in order) *)
let generate_csk_query k =          (* assert k >= 2 *)
  let q1 = "select cs1.target as target, cs1.target_addr as target_addr,\n" in
  let qcallstring =
    let percents = List.init k ~f:(fun _ -> "%s") |> String.concat ~sep:"," in
    let csi = List.init k ~f:(fun i -> sprintf ", cs%d.cs" (i + 1))
              |> String.concat ~sep:"" in (* reversed due to question 4 *)
    sprintf "rtrim(printf(\"%s\"%s), \",\") as cs\n" percents csi
  in
  let q3 = "from cs1\n" in
  let qjoin = List.init (k - 1) ~f:begin fun i ->
      sprintf "left join cs1 as cs%d on cs%d.head = cs%d.target\n"
        (i + 2) (i + 1) (i + 2)
    end |> String.concat ~sep:""
  in
  let qorder = "order by target, target_addr" in (* optional *)
  q1 ^ qcallstring ^ q3 ^ qjoin ^ qorder

(* create view (but we can also create a table to cache the result) *)
let create_csk k db =
  let csk = sprintf "cs%d" k in
  let query = generate_csk_query k in
  (* eprintf "%s\n" query; *)
  Sqlite3EZ.exec db (sprintf "create view %s as %s" csk query)

(* print csk to stdout *)
let print_callstring k db =
  let csk = sprintf "cs%d" k in
  let stmt = Sqlite3EZ.make_statement db ("select * from " ^ csk) in
  Sqlite3EZ.statement_query stmt [||]
    begin fun data ->
      let target = Sqlite3EZ.Data.to_string data.(0) in
      let target_addr = Sqlite3EZ.Data.to_string data.(1) in
      let csk = Sqlite3EZ.Data.to_string data.(2) in
      printf "%s:%s <= %s\n" target target_addr csk
    end (fun _ _ -> ()) ()

module GenericDotDumper = struct
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name name = "\"" ^ name ^ "\""
    let vertex_attributes _ = []
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
end

let compute_calls_scc db =
  let module V = String in
  let module E = struct
    include String
    let default = "0"
  end in
  let module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(V)(E) in
  let module Dot = Graph.Graphviz.Dot(struct (* for debugging *)
      include G
      include GenericDotDumper
    end)
  in
  (* insert from calls into g *)
  let query = "select from_func, from_addr, to_func from calls" in
  let stmt = Sqlite3EZ.make_statement db query in
  let g = Sqlite3EZ.statement_query stmt [||]
      begin fun data ->
        let from_func = data.(0) |> Sqlite3EZ.Data.to_string in
        let from_addr = data.(1) |> Sqlite3EZ.Data.to_string in
        let to_func = data.(2) |> Sqlite3EZ.Data.to_string in
        (from_func, from_addr, to_func)
      end
      begin fun e g -> G.add_edge_e g e end
      G.empty
  in
  Out_channel.with_file "pre.dot" ~f:begin fun out ->
    Dot.output_graph out g
  end;
  let module SCC = Graph.Components.Make(G) in
  let _num_scc, f = SCC.scc g in
  let ors = Array.map (SCC.scc_array g) ~f:(String.concat ~sep:"|") in
  let module Con = Graph.Contraction.Make(G) in
  let g = Con.contract (fun (u, _, v) -> f u = f v) g in
  let g = G.map_vertex (fun u -> sprintf "%s_%d" ors.(f u) (f u)) g in
  Out_channel.with_file "post.dot" ~f:begin fun out ->
    Dot.output_graph out g
  end;
  (* insert g into calls_scc *)
  let query = "insert into calls_scc values (null, ?, ?, ?)" in
  let stmt = Sqlite3EZ.make_statement db query in
  G.iter_edges_e begin fun (u, label, v) ->
    Sqlite3EZ.(statement_exec stmt [|
        Data.TEXT u; Data.TEXT label; Data.TEXT v
    |])
  end g

let main project =
  let db_filename = "exe.db" in
  (try Unix.unlink db_filename with _ -> ());
  Sqlite3EZ.with_db db_filename begin fun db ->

    (* extract call relationship using BAP *)
    insert_functions project db;

    (* if SCC, then replace the calls table with calls_scc *)
    let calls_table = ref "calls" in
    (try
       if Unix.getenv "SCC" = "true" then
         compute_calls_scc db;
       calls_table := "calls_scc"
     with Not_found -> ());

    (* generate callstring using the correct table *)
    create_csk 3 db;
    print_callstring 3 db;    (* hardcoded constant *)
  end;
  project

let () = register main
