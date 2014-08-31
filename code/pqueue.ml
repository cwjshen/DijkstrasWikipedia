open Core.Std

(* code borrowed heavily from 
   https://github.com/c-cube/ocaml-containers/blob/master/heap.ml
   Copyright (c) 2013, Simon Cruanes
   All rights reserved. *)
module type PRIORITYQUEUE = 
  sig

    exception EmptyQueue

    (* A heap with type 'a elt *)
    type 'a t

    (* Generate an empty heap *)
    val empty : ('a -> 'a -> int) -> 'a t

    (* Check if heap is empty *)
    val is_empty : 'a t -> bool

    (* Push elt into heap *)
    val insert : 'a t -> 'a -> unit

    (* Return and remove minimum value *) 
    val remove : 'a t -> 'a

    (* Find element in tree *)
(*  val find : 'a t -> 'a -> bool *)

  end


module Minheap : PRIORITYQUEUE =
  struct

    exception EmptyQueue

    (* Tree heap *)
    type 'a t = {
      mutable tree : 'a tree;
      cmp : 'a -> 'a -> int
    }
    and 'a tree = | Empty | Node of ('a tree * 'a * 'a tree)

    (* Generate an empty heap *)
    let empty f = { tree = Empty; cmp = f; }


    (* Check if heap is empty *)
    let is_empty h = h.tree = Empty


    (* Insert element into the tree *)
    let insert h e =
      let rec split cmp p tree =
	match tree with
	| Empty -> Empty, Empty
	| Node (l, e, r) ->
	   if cmp e p <= 0 then
	     (match r with
              | Empty -> (tree, Empty)
              | Node (r1, y, r2) ->
		 if cmp y p <= 0
		 then
		   let small, big = split cmp p r2 in
		   let lnode = Node (l, e, r1) in
		   Node (lnode, y, small), big
		 else
		   let small, big = split cmp p r1 in
		   Node (l, e, small), Node (big, y, r2))
	   else
	     (match l with
	      | Empty -> (Empty, tree)
	      | Node (l1, y, l2) ->
		 if cmp y p <= 0
		 then
		   let small, big = split cmp p l2 in
		   Node (l1, y, small), Node (big, e, r)
		 else
		   let small, big = split cmp p l1 in
		   let rnode = Node (l2, e, r) in
		   small, Node (big, y, rnode))
      in
      let small, big = split h.cmp e h.tree in
      let new_tree = Node (small, e, big) in
      h.tree <- new_tree
		  
    (* Get minimum value and remove it from the tree *)
    let remove h =
      let rec remove_min tree = 
	match tree with
	| Empty -> raise EmptyQueue
	| Node (Empty, e, r) -> e, r
	(* Rebalance tree *)
	| Node (Node (Empty, e, r), e2, r2) -> e, Node (r, e2, r2) 
	| Node (Node (l, e, r), e2, r2) ->
	   let m, l' = remove_min l in
	   m, Node (l', e, Node (r, e2, r2))
      in
      let m, new_tree = remove_min h.tree in
      h.tree <- new_tree;
      m

    (* Check if element is in tree *)
(*  let find h x = 
      let rec find_elt tree =
	match tree with
	| Empty -> false
	| Node (l, e, r) -> 
	   if h.cmp x e = 0 then true 
	   else if h.cmp x e < 0 then find_elt l
	   else find_elt r
      in find_elt h.tree *)

  end


(* Testing code *)
(*
module Test : PRIORITYQUEUE = Minheap

let h = Test.empty () ;;

let _ = Test.insert h 5
assert(Test.is_empty h = false);;

assert(Test.remove h = 5);;

let _ = Test.insert h 10
let _ = Test.insert h 3
assert(Test.remove h = 3);;

assert(Test.find h 10 = true);;
assert(Test.find h 15 = false);;
 *)
