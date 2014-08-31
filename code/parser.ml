open Core.Std
open Graph

let create_graph (f: string) : Graph.graph =

	       
  (* creates input channel *)
  let chan = In_channel.create f in
  
  (* gets rid of 1st line *)
  ignore (In_channel.input_line chan);
			
  (* creates an empty graph *)
  let simplewikigraph = ref Graph.empty in

  (* function to perform on each line of the file *)
  (* adds an edge to the graph *)
  let f_on_line (s: string) : unit =
    (* splits the string on the tab character *)
    let (node,neighbor) = String.lsplit2_exn s ~on:'\t' in
    let a = !simplewikigraph in
    (* adds edge to node *)
    simplewikigraph := Graph.add_edge a node neighbor 1
  in

  (* iterates over each line in the file, adding an edge to the graph *)
  In_channel.iter_lines chan ~f:(f_on_line);

  !simplewikigraph

let create_table (f: string) : LookupTable.table =
  (* creates input channel *)
  let chan = In_channel.create f in
  
  (* gets rid of 1st line *)
  ignore (In_channel.input_line chan);
			
  (* creates an empty table *)
  let simpletable = ref LookupTable.empty in

  (* function to perform on each line of the file *)
  (* adds key to the lookup table *)
  let f_on_line (s: string) : unit =
    let a = !simpletable in
    simpletable := LookupTable.add_key a s
  in

  (* iterates over each line in the file, adding a key to the lookup table *)
  In_channel.iter_lines chan ~f:(f_on_line);

  !simpletable
