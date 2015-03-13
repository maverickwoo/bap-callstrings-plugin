open Core_kernel.Std
open Bap.Std
open Or_error
open Program_visitor

(* given a exe.db with a table called "calls" and nothing else,
   generate the callstring tree of a given root *)

type sym = string with sexp
type tree =
  | T of (sym * Int64.t)
  | E of sym                    (* don't know how to use it *)
  | R of (sym * Int64.t) * sym
  | N of (sym * Int64.t) * tree list
with sexp

let falabel (f, a) = sprintf "%s:%Ld" f a

let get_children n db =
  let query = sprintf "select from_addr, to_func from calls where from_func = '%s'" n in
  let stmt = Sqlite3EZ.make_statement db query in
  Sqlite3EZ.statement_query stmt [||]
    begin fun data ->
      let from_addr = Sqlite3EZ.Data.to_string data.(0) in
      let to_func = Sqlite3EZ.Data.to_string data.(1) in
      (to_func, Int64.of_string from_addr)
    end (fun a acc -> a::acc) []

(* at node r called from address ra  *)
let rec gen_tree r ra path db =
  let children = get_children r db in
  match children with
  | [] -> T (r, ra)
  | _ ->
    let rpath = r :: path in
    let trees = List.map children ~f:begin fun (f, a) ->
        if List.mem rpath f then
          R ((r, a), f)
        else
          gen_tree f a rpath db
      end in
    N ((r, ra), trees)

let main project =
  let db_filename = "exe.db" in
  let root = Unix.getenv "ROOT" in
  Sqlite3EZ.with_db db_filename begin fun db ->

    (* q4: check if calls table exist; if not, reconstruct from cs3 table *)
    let _recon_query = "
insert into calls
select null,
       substr(cs1, 1, instr(cs1, \":\") - 1) as from_func,
       substr(cs1, instr(cs1, \":\") + 1) as from_addr,
       target as to_func,
       target_addr as to_addr
from (
select distinct target, target_addr,
        substr(cs||\",\", 1, instr(cs||\",\", \",\") - 1) as cs1 from cs3
)
" in

    gen_tree root 0L [] db |> sexp_of_tree
    |> Sexp.to_string_hum ~indent:2 |> print_endline
  end;
  project

let () = register main
