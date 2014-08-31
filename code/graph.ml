open Core.Std
open Sexplib.Sexp

module StringKey : (Map.Key with type t = string) =
struct
  type t = string

  let t_of_sexp sexp =
    match sexp with
    | Atom(s) -> s
    | List(_) -> ""

  let sexp_of_t s = Atom(s) 
  let compare = String.compare
 
end

(* does not have delete_node or delete_edge functions *)
(* dijkstras algorithm would not conceivably use these functions *)
module Graph =
  struct
    module StringDict = Map.Make(StringKey)
    type node = StringKey.t
    type graph =  (node * int) list StringDict.t
    
    let empty = StringDict.empty
    let is_empty (g: graph) : bool =
      StringDict.is_empty g

    let has_node (g: graph) (n: node) : bool =
      StringDict.mem g n
    let add_node (g: graph) (n: node) : graph =
      if has_node g n
      then failwith "node already in graph"
      else StringDict.add g ~key:n ~data:[]
    (* if src or dst do not exist, then they are added by calling add_node*)
    (* if the edge already exists, throw exception *)
    let rec add_edge (g: graph) (src: node) (dst: node) (w: int) : graph =
      if has_node g src && has_node g dst
      then
	let srcneighbors = StringDict.find_exn g src in
	if List.mem ~equal:(fun (a,_) (c,_) -> a = c) srcneighbors (dst,w)
	then failwith "edge already exists"
	else StringDict.add_multi g ~key:src ~data:(dst,w)
      else 
	if has_node g src
	then add_edge (add_node g dst) src dst w
	else add_edge (add_node g src) src dst w
    let change_weight (g: graph) (src: node) (dst: node) (w: int) : graph =
      if not (has_node g src)
      then failwith "source does not exist"
      else
	if not (has_node g dst)
	then failwith "destination does not exist"
	else 
	  let srcneighbors = StringDict.find_exn g src in
	  if List.mem ~equal:(fun (a,_) (c,_) -> a = c) srcneighbors (dst,w)
	  then StringDict.add
		 g
		 ~key:src
		 ~data:((dst,w) :: (List.fold
				      srcneighbors
				      ~init:[]
				      ~f:(fun a (b,c) ->
					  if b = dst
					  then a
					  else (b,c) :: a)))
	  else failwith "edge does not exist"

    let get_nodes (g: graph) : node list = 
      StringDict.fold_right g ~init:[] ~f:(fun ~key:k ~data:_ a -> k :: a)
    (* returns neighbors and their weights *)
    let get_neighbors (g: graph) (n: node) : (node * int) list  =
      if not (has_node g n)
      then failwith "node does not exist"
      else
        StringDict.find_exn g n
    let get_weight (g: graph) (src: node) (dst: node) : int =
      if not (has_node g src)
      then failwith "source does not exist"
      else
	if not (has_node g dst)
	then failwith "destination does not exist"
	else
	  let srcneighbors = StringDict.find_exn g src in
	  match List.find srcneighbors ~f:(fun (a,_) -> a = dst) with
	  | Some (_,b) -> b
	  | None -> failwith "edge does not exist"
  end

module LookupTable = 
struct

  module StringDict = Map.Make(StringKey)
  type key = StringKey.t
  type table = (int * StringKey.t * bool) StringDict.t

  let empty = StringDict.empty
  let is_empty (t: table) : bool = 
    StringDict.is_empty t

  let add_key (t: table) (k: key) : table = 
    if StringDict.mem t k
    then failwith "key already in table"
    (* empty string cannot be a node, 
       therefore setting initial previous node to empty string 
       signifies that there is no initial previous node *)
    else 
      StringDict.add t ~key:k ~data:(Int.max_value, "", false) 

  let update_value (t: table) (k: key) (v: (int * StringKey.t * bool)) : table = 
    if not (StringDict.mem t k)
    then failwith "key does not exist"
    else
      StringDict.add t ~key:k ~data:v

  let get_value (t: table) (k: key) : (int * StringKey.t * bool) =
    if not (StringDict.mem t k)
    then failwith "key does not exist"
    else
      StringDict.find_exn t k

end
(*
(* we decided to not seal Graph *)
module type GRAPH =
sig
  type graph
  type node
	 
  val empty : graph
  val is_empty : graph -> bool
			    
  val has_node : graph -> node -> bool
  val add_node : graph -> node -> graph
  val add_edge : graph -> node -> node -> int -> graph
  val change_weight : graph -> node -> node -> int -> graph


  val get_nodes : graph -> node list	
  val get_neighbors : graph -> node -> node list
  val get_weight : graph -> node -> node -> int 			      
end

(* testing for Graph *)
let a = Graph.empty;;
Graph.is_empty a;;
(*Graph.has_node a 5;; (* should break *)*)
Graph.has_node a "key";; (* false *)
let a = Graph.add_node a "key";;
Graph.has_node a "key";; (* true *)
let a = Graph.add_edge a "key2" "key3" 5;;
Graph.has_node a "key2";; (* true *)
Graph.has_node a "key3";; (* true *)
Graph.has_node a "key4";; (* false *)
let a = Graph.add_edge a "key2" "key3" 5;; (* should failwith edgealreadyexists *)
let a = Graph.add_node a "key2";; (* should failwith nodealreadyexists *)
let a = Graph.add_edge a "key" "key2" 3;;
let a = Graph.add_edge a "key4" "key" 4;;
let a = Graph.add_edge a "key3" "key5" 7;;
let a = Graph.add_edge a "key3" "key2" 8;;
let a = Graph.change_weight a "key2" "key3" 5;; (* its already 5 *)
let a = Graph.change_weight a "key3" "key5" 1;; (* from 7 to 1 *)
let a = Graph.change_weight a "key20" "key5" 3;; (* should failwith src doesnt exist *)
let a = Graph.change_weight a "key5" "key20" 3;; (* should failwith dst doesnt exist *)
let a = Graph.change_weight a "key" "key4" 3;; (* should failwith edge doesnt exist *)
Graph.get_nodes a;; (* should return ["key"; "key2"; "key3"; "key4"; "key5"] *)
Graph.get_neighbors a "key5";; (* should return [] *)
Graph.get_neighbors a "key20";; (* should failwith node doesnt exist *)
Graph.get_neighbors a "key";; (* should return ["key2"] *)
Graph.get_neighbors a "key3";; (* should return ["key2"; "key5"] but order unknown *)
Graph.get_weight a "key20" "key5";; (* should failwith src doesnt exist *)
Graph.get_weight a "key5" "key20";; (* should failwith dst doesnt exist *)
Graph.get_weight a "key" "key4";; (* should failwith edge doesnt exist *)
Graph.get_weight a "key4" "key";; (* should return 4 *)
Graph.get_weight a "key" "key2";; (* should return 3 *)
Graph.get_weight a "key2" "key3";; (* should return 5 *)
Graph.get_weight a "key3" "key2";; (* should return 8 *)
Graph.get_weight a "key3" "key5";; (* should return 1, not 7 *)
 *)
