open Core_kernel.Std
open Bap.Std
open Program_visitor

module G = struct
  type t = project
  module V = struct
    type t = string
  end
  module E = struct
    type t = string * string
    let src, dst = fst, snd
  end
  let iter_vertex f t = Table.iter t.symbols ~f
  let iter_edges_e f t =
    Table.iteri t.symbols ~f:begin fun mem src ->
      Disasm.insns_at_mem t.program mem |>
      Seq.iter ~f:begin fun (_mem, insn) ->
        Bil.iter (object inherit [unit] Bil.visitor
          method! enter_int addr () =
            if in_jmp then
              match Table.find_addr t.symbols addr with
              | None -> ()
              | Some (mem, dst) ->
                if Addr.(Memory.min_addr mem = addr) then f (src, dst)
        end) (Insn.bil insn)
      end
    end
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_name v = sprintf "%S" v
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end

let main project =
  let module Dot = Graph.Graphviz.Dot(G) in
  Out_channel.with_file "callgraph.dot" ~f:begin fun out ->
    Dot.output_graph out project
  end;
  project

let () = register main
