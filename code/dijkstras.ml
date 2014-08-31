open Core.Std
open Pqueue
open Graph
(*
let edge_file = "shorttitleslinks.txt"
let page_file = "shortnodes.txt"
 *)
let edge_file = "titleslinks.txt"
let page_file = "nodes.txt"

let edge_graph = Parser.create_graph edge_file
let lookup_table = Parser.create_table page_file

module Pqueue : PRIORITYQUEUE = Minheap
let compare (_,b) (_,d) = compare b d

let dijkstras (s : Graph.node) (t : Graph.node) (g: Graph.graph)
	      (t2: LookupTable.table) : (int * string list) option =
  (* check if the both the start and end nodes are in the graph *) 
  if not (Graph.has_node g s && Graph.has_node g t) 
  (* return None if they aren't *)
  then failwith "start or end node not in graph"
  else 
    (let pqueue = Pqueue.empty compare in
     let tableref = ref t2 in
     
     (* add sn's neighbors to the priorityqueue *)
     let sneighbors = Graph.get_neighbors g s in
     List.iter sneighbors ~f:(fun (n,w) -> Pqueue.insert pqueue (n,w));
     (* update the parallel dict *)
     List.iter sneighbors 
	       ~f:(fun (n,w) -> 
		   let table = !tableref in
		   tableref := 
		     LookupTable.update_value table n (w, s, false));
     (* returns the path from s to t in reverse order *)
     let rec get_path t n s mt =
       if mt <= 0
       then failwith "no path found (should never happen)"
       else
	 (let (_,b,_) = LookupTable.get_value t n in
	  if b = s
	  then [b]
	  else b :: (get_path t b s (mt-1)))
     in
     (* main looping function in dijkstra's *)
     let rec dijloop () =
       (* if priorityqueue is empty, return None *)
       if Pqueue.is_empty pqueue
       then failwith "no path found"
       else
	 (* remove min node from priorityqueue *)
	 (* the value inside _ here should agree with w_from_s, from table *)
	 (let (min,_) = Pqueue.remove pqueue in
	  let table = !tableref in
	  let (w_from_s,b,visited) = LookupTable.get_value table min in
	  if visited then dijloop ()
	  else
	    (* check if it is tn *)
	    (if min = t
	     (* if it is, call get_path and return (the path, the length a) *)
	     then Some (w_from_s, List.rev (min :: (get_path table min s 1000)))
	     else
	       (* set visited flag to true *)
	       (tableref := LookupTable.update_value
			      table min (w_from_s,b,true);
		(* get neighbors of min *)
		let min_neighbors = Graph.get_neighbors g min in
		(* iterate on each neighbor *)
		List.iter
		  min_neighbors
		  ~f:(fun (nb,w_from_min) ->
		      (* gets the most updated versino of our table *)
		      let table = !tableref in
		      (* checks if there is a shorter path to the nb *)
		      let (nb_w_from_s,_, nb_v) =
			LookupTable.get_value table nb in
		      let dist_sum = (* min's *) w_from_s + w_from_min in
		      if nb_w_from_s > dist_sum
		      then
			(* if there is then add shorter path to queue *)
			(Pqueue.insert pqueue (nb, dist_sum);
			 (* and also update the lookuptable *)
			 tableref := LookupTable.update_value
					  table nb (dist_sum,min,nb_v)));
		dijloop () )))
     in
     (* loop *)
     dijloop () )

match dijkstras Sys.argv.(1) Sys.argv.(2) edge_graph lookup_table with
| None -> print_string "No path found.\n"
| Some (a,b) -> 
   print_string ("Distance: " ^ string_of_int a ^ "\n" ^ 
		   "Path: " ^ (String.concat ~sep:", " b) ^ "\n")
