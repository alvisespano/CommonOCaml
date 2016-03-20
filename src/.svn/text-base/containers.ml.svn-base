(*
 * Common library
 * containers.ml: some useful containers
 *
 * (C) 2007 Alvise Spanò @ H-Care srl.
 *)
 
(** Polymorphic container library. *)
 
open Prelude 

(** Polymorphic dynamic buffer. Extensible linear container that grows dynamically.
*) 
class ['a] dynbuffer size k =
  object (self)
    val mutable l = []
    val mutable last = Array.make size k
    val mutable iti = 0
    val mutable len = 0
  
    method private step =
        if iti = size - 1 then begin
            l <- l @ [last];
            last <- Array.make size k;
            iti <- 0;
          end
        else iti <- iti + 1

    (** Append [x] to the tail of the container. *)
    method append x =
        last.(iti) <- x;
        len <- len + 1;
        self#step
    
    (** Get the number of elements in the container. *)
    method length = len

	(** Get all the elements in the container. *)
	method to_list = List.rev (self#fold (fun acc x -> x :: acc) [])

	(** Get all the elements in the container. *)
	method to_array =
		let a = Array.make len k 
		in
			ignore (self#fold (fun i x -> a.(i) <- x; (i + 1)) 0);
			a
    
	(** Get the [i]-th element. *)
    method get i =
        if i >= len then raise (Invalid_argument "Containers.dynbuffer#get: index out of bounds")
        else
            let n = i / size
            in
                (if n = len / size then last else XList.nth l n).(i mod size)
    
    (** Iterate function [f] over all elements of the container. *)
    method iter f = if len = 0 then () else self#iter_from 0 f
    
	(** Get elements from position [a] to [b]. *)
	method range (a, b) = 
		if a < 0 || b < a then raise (Invalid_argument "Containers.dynbuffer#range: invalid bounds");
		let rec f acc = function
			0	-> acc
		  | n 	-> f ((self#get (a + n - 1)) :: acc) (n - 1)
		in
			f [] (b - a + 1)

    (** Fold the container in the usual functional fashion for lists. *)
    method fold : 'z. ('z -> 'a -> 'z) -> 'z -> 'z = fun f z -> if len = 0 then z else self#fold_from 0 f z
    
    (** Iterate function [f] from position [i] to the end of the container. *)
    method iter_from i f = self#fold_from i (fun () x -> f x) ()
    
    (** Fold from position [i] to the end of the container. *)
    method fold_from : 'z. int -> ('z -> 'a -> 'z) -> 'z -> 'z = fun i f z ->
        if i >= len then raise (Invalid_argument "Containers.dynbuffer#fold_from: index out of bounds")
        else
            let fold_subarray i len f z a =
                let rec recur n z = if n < len then recur (n + 1) (f z a.(i + n)) else z
                in
                    recur 0 z
            in
            let aa =
                let rec recur n = function
                    []             -> []
                  | (_ :: aa) as r -> if n = 0 then r else recur (n - 1) aa
                in
                    recur (i / size) l
            in
                match aa with
                    []      -> let i = i mod size in fold_subarray i ((len - i) mod size) f z last
                  | a :: aa ->
                        let z = let i = i mod size in fold_subarray i (size - i) f z a in
                        let z = List.fold_left (Array.fold_left f) z aa
                        in
                            fold_subarray 0 (len mod size) f z last
            
  end

type 'a node = { 
	mutable prev	: 'a node option;
	mutable next	: 'a node option;
	contents 		: 'a;
}

(** Double linked list. Insertion and appending are O(K)
*) 

class ['a] dlist =
  object (self)
	val mutable hd = None
	val mutable tail = None
	val mutable length = 0 

	method insert (x : 'a) =
		length <- length + 1;
		match hd with 
			None	-> 
				let node = {	
					prev 		= None;
					next 		= None;
					contents	= x;
				} 
				in 
					hd 		<- Some node;
					tail 	<- Some node
		  | Some hd'	->
				let node = {
					prev 		= None;
					next 		= hd;
					contents	= x;
				} 
				in
					hd'.prev 	<- Some node;
					hd 			<- Some node

	method append (x : 'a) = 
		length <- length + 1;
		match tail with 
			None	-> 
				let node = {	
					prev 		= None;
					next 		= None;
					contents	= x;
				} 
				in 
					hd 		<- Some node;
					tail 	<- Some node
		  | Some tail'	->
				let node = {
					prev 		= tail;
					next 		= None;
					contents	= x;
				} 
				in
					tail'.next 	<- Some node;
					tail		<- Some node

	method fold_left : 'b. ('b -> 'a -> 'b) -> 'b -> 'b = fun f acc ->
		let rec g acc = function
			None	-> acc
		  | Some x	-> g (f acc x.contents) x.next
		in
			g acc hd

	method fold_right : 'b. ('b -> 'a -> 'b) -> 'b -> 'b = fun f acc ->
		let rec g acc = function
			None	-> acc
		  | Some x	-> g (f acc x.contents) x.prev
		in
			g acc tail

	method to_list_rev = self#fold_left (fun acc x -> x :: acc) []
	method to_list = self#fold_right (fun acc x -> x :: acc) []  

  end

(** Synced hashtable exception free
*)
class ['key, 'data] sync_hashtable size =
  object (self)
	val tbl : ('key, 'data) Hashtbl.t XThread.sync = new XThread.sync (Hashtbl.create size)
	
	method add (k : 'key) (x : 'data) = tbl#apply (fun tbl -> Hashtbl.add tbl k x)
	method remove (k : 'key) = tbl#apply (fun tbl -> try Hashtbl.remove tbl k with _ -> ())
	method find (k : 'key) = try tbl#apply (fun tbl -> Some (Hashtbl.find tbl k)) with Not_found -> None
	method exists (k: 'key) = tbl#apply (fun tbl -> Hashtbl.mem tbl k)
	method length = tbl#apply Hashtbl.length

	method to_list = 
		let f k x acc = (k, x) :: acc 
		in		
			tbl#apply (fun tbl -> Hashtbl.fold f tbl [])

	method of_list l = List.iter (fun (k, x) -> self#add k x)  l
  end

(** Hashtable exception free
*)
class ['key, 'data] hashtable size =
  object (self)
	val tbl : ('key, 'data) Hashtbl.t = Hashtbl.create size	
	method add (k : 'key) (x : 'data) = Hashtbl.add tbl k x
	method remove (k : 'key) = try Hashtbl.remove tbl k with _ -> ()
	method find (k : 'key) = try Some (Hashtbl.find tbl k) with Not_found -> None
	method exists (k: 'key) = Hashtbl.mem tbl k
	method length = Hashtbl.length tbl
	method to_list = 
		let f k x acc = (k, x) :: acc 
		in		
			Hashtbl.fold f tbl []
	method of_list l = List.iter (fun (k, x) -> self#add k x)  l
  end
