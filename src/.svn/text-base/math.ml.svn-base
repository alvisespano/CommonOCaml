(*
 * Common library
 * math.ml: mathematical functions and utils
 * 
 * (C) 2004 Quendi srl.
 *)

(** Mathematic bits. *)

DEFINE MATH_CHECK
 
open Prelude
open Printf
open List
open XList
 
(* function checkers *)

let (>=<) x (a, b) = x >= a && x <= b

let check b e msg = 
	IFDEF MATH_CHECK THEN if b then () else e msg
	ELSE ()
	END

let check_fun name f error ?(dx = 1.0) (x0, x1) (y0, y1) =
	IFDEF MATH_CHECK THEN
	    let check x =
	        let y = f x
	        in
	            check (y >=< (y0, y1)) error (sprintf "%s(%g) = %g is out of codomain [%g, %g]" name x y y0 y1)
	    in
	      begin
	        ffor check (x0, x1) dx;
	        check (f x1)
	      end
	ELSE ()
	END
      

(* constants *)
 
let pi = acos (-1.0)
let rad = pi /. 180.0

(* rebindings and shortcuts *)

let fabs = abs_float
let fmod = mod_float
let sin x = sin (x *. rad)
let cos x = cos (x *. rad)


(* utils *)

let compare_float epsilon a b = let d = a -. b in if abs_float d < epsilon then 0 else if d < 0. then -1 else 1

let sgn x = if x < 0.0 then -1.0 else 1.0

let k (x : float) = fun _ -> x

let at_least a x = if x < a then a else x

let no_more_than b x = if x > b then b else x

let crop (a, b) x = 
    let (a, b) = Pair.sort compare (a, b) in
        if x < a then a else if x > b then b else x

let proj (a', b') (a, b) x = 
    if a' = b' then a'  
    else (x -. a) *. ((b' -. a') /. (b -. a)) +. a'

let fround x = if x -. floor x > 0.5 then ceil x else floor x

let round x = int_of_float (fround x)

let quantize q x = fround (x /. q) *. q

let compress x = (-. (1.0 /. (fabs x +. 1.0)) +. 1.0) *. sgn x 

let perturb errore ?(q = 0.0) x = Random.float (2.0 *. errore) -. errore +. q +. x


(* functions *)

type daf = (float * float) * (float -> float)   (* (domain, function) *)

type distr = float * float                      (* (probabilty, value) *)

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let periodic f per = fun x -> let x = fmod x per in f (if x >= 0.0 then x else per +. x)   

let dephase f phi = fun x -> f (x -. phi)

let linear m q = fun x -> m *. x +. q

let abscissa = k

let projf (x0', y0', x1', y1') (x0, y0, x1, y1) f x =
    proj (y0', y1') (y0, y1) (f (proj (x0, x1) (x0', x1') x))

let transl f (dx, dy) = fun x -> (f (x -. dx)) +. dy

let absf f x = fabs (f x)

let quantizef f q x = quantize q (f x)

let segment p0 p1 =
    let ((x0, y0), (x1, y1)) = Pair.sort Pair.comparefst (p0, p1) in
    let m = (y1 -. y0) /. (x1 -. x0) in
    let q = y0 -. m *. x0
    in
        linear m q

let curve p0 p1 =
    let ((x0, y0), ((x1, y1))) = Pair.sort Pair.comparefst (p0, p1)
    in
        fun x ->
            if x < x0 then y0 else if x > x1 then y1
            else projf (x0, y0, x1, y1) (-90.0, -1.0, 90.0, 1.0) sin x

(* juxt domains and functions *)
let juxt_dafs default dafs x =
    try (assocp (fun (x0, x1) -> x0 <= x && x <= x1) dafs) x
    with Not_found -> default

(* juxt points and joins // not sorted *)
let juxt_pajs default pajs x = 
	let g ((((x0, _) as p0), ((x1, _) as p1)), f) = ((x0, x1), (f p0 p1)) in
	let dafs = List.map g pajs 
	in 
		juxt_dafs default dafs x	

(* juxt points and joins, with starting point *)
let link_points' default pfs ?(p0 = (0.0, 0.0)) x =
	let h ((a, _), _)  ((b, _), _) = if a < b then -1
									 else if a = b then 0
									 else 1 
	in
	let pfs = List.sort h pfs in	
	let rec f pfs p0 = match pfs with
		[]		    			   -> []
	   |(p1, g) :: els -> ((p0, p1), g)::(f els p1)
	in 
		let pajs = f pfs p0 
		in
			juxt_pajs default pajs x


let link_points f default = function
    []   -> abscissa default
  | ps   -> let ps = sort Pair.comparefst ps in
	        let f (dafs, ((x0, _) as p0)) ((x1, _) as p1) =
                if x0 = x1 then (dafs, p1)
                else (((x0, x1), f p0 p1) :: dafs, p1)
            in
                juxt_dafs default (fst (fold_left f ([], hd ps) (tl ps)))
        
let step p0 p1 =
    let ((x0, y0) as p0, ((x1, y1) as p1)) = Pair.sort Pair.comparefst (p0, p1)
    in
	    fun x -> if x < x0 then y0 else if x > x1 then y1 else segment p0 p1 x

	    
let make_prob_distr distrs =
    let f (distrs, tot) (p, x) = let tot = tot +. p in ((tot, x) :: distrs, tot)
    in
        List.sort Pair.comparefst (fst (List.fold_left f ([], 0.0) distrs))
        
let prob_distr default distrs =
    let distrs = make_prob_distr distrs
    in
        fun x -> try assocp (fun p -> x <= p) distrs with Not_found -> default

let rnd_prob_distr default distrs = 
	let x = Random.float 1.0 in
		fun () -> prob_distr default distrs x

let normalize_probs ?(rng = (0.0, 1.0)) distrs =	
	let tot = List.fold_left (+.) 0.0 (List.map fst distrs)
	in
		List.map (fun (prob, win) -> (proj (fst rng, snd rng) (0.0, tot) prob), win) distrs

		
let balance f (a, b) median =
    let mid = (a +. b) /. 2.0
    in
        fun x ->
            let y = f x
            in
                if y < mid then proj (a, median) (a, mid) y
                else proj (median, b) (mid, b) y
                
let balance' (a, b, mid) median x =
	if x < mid 
		then proj (a, median) (a, mid) x
        else proj (median, b) (mid, b) x
        
let balance_prob_distr (a, b, mid) median distrs =
	let f (prob, win) = (balance' (a, b, mid) median prob, win)
	in
		List.map f distrs

                
let perturbf f var x = perturb var (f x)

let sparse w default prob x = if Random.float 1.0 <= prob then w x else default

let arc (rx, ry) =
    let f x = sqrt (1.0 -. (x ** 2.0))
    in
        projf (0.0, 0.0, rx, ry) (0.0, 0.0, 1.0, 1.0) f

let prob x p = sparse ident 0.0 p x 


(* waves *)

let sawfish =
    let f = link_points segment 0.0 [(0.0, 0.0); (180.0, 1.0); (180.0, -1.0); (360.0, 0.0)]
    in
        periodic f 360.0

let triangular =
    let f = link_points segment 0.0 [(0.0, 0.0); (90.0, 1.0); (270.0, -1.0); (360.0, 0.0)]
    in
        periodic f 360.0

let spire =
    let a1 = transl (arc (90.0, -1.0)) (0.0, 1.0) in
    let a2 = transl (arc (-90.0, -1.0)) (180.0, 1.0) in
    let a3 = transl (arc (90.0, 1.0)) (180.0, -1.0) in
    let a4 = transl (arc (-90.0, 1.0)) (360.0, -1.0) in
    let f = juxt_dafs 0.0 [((0.0, 90.0), a1); ((90.0, 180.0), a2); ((180.0, 270.0), a3); ((270.0, 360.0), a4)]
    in
        periodic f 360.0

let pwave w ?(amp = 1.0) ?(pulse = 1.0) ?(phi = 0.0) ?(q = 0.0) x =
    amp *. (w (pulse *. x +. phi)) +. q

let pfwave w ?(amp = k 1.0) ?(pulse = k 1.0) ?(phi = k 1.0) ?(q = k 0.0) x =
    (amp x) *. w ((pulse x) *. x +. (phi x)) +. (q x)


(* Array utilities *)

let average l = 
	let (n, tot) = List.fold_left (fun (n, tot) x -> (n + 1, tot +. x)) (0, 0.0) l 
	in
		tot /. (float_of_int n)

let variance l =
	let n = float_of_int (List.length l) in
	let m = average l in
	let y = List.fold_left (fun acc x -> acc +. ((x -. m) ** 2.0)) 0.0 l in
		y /. n

let sqm l = sqrt (variance l)

let aproj f a freq default =
	let n = Array.length a in
	let n' = int_of_float (ceil ((float_of_int n) /.  freq)) in
	let a' = Array.make n' default in
	let g = fun i x -> 
		let i' = int_of_float ((float_of_int i) /. freq) in
		let x' = a'.(i') in
			a'.(i') <- (f x' x)
	in
        Array.iteri g a;
		a'
