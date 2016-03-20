(*
 * Angband
 * Common daemon library
 * security.ml: security, hashing and zipping
 *
 * (C) 2005 Quendi srl.
 *)

(** Security, hashing, cryptographic and zipping facilities. This module is based on the Cryptokit library. *)

open Prelude
open Cryptokit

(** Type for base64 encoded data. *)
type 'a base64 = Base64 of string

(** Type for compressed data. *)
type 'a zip = Zip of string

(** Type for encrypted data. *)
type 'a cipher = Cipher of string

(** Type for authenticated data. *)
type 'a auth = Auth of string * 'a cipher

(* Type for secret codes. A secret is a function that evaluates a secret code string used on cryptographic algorithms. Such codes are evaluated
each time they are used and the temporary data produced is wiped immediately from memory.
*)
type secret = Secret of (unit -> string)

(** Creates a secret given a constant string. *)
let secret_of_string s =
	let rec f s =
		if String.length s >= 24 then String.sub s 0 24
		else
			let h = Cryptokit.MAC.hmac_sha1 s in
			let s' = s ^ (Cryptokit.hash_string h s)
			in
				f s'
	in
		Secret (fun () -> f s)
					
(** Hash [x] given a secret. *)					
let hash (Secret f) x =
	let key = f () in
	let r = hash_string (MAC.triple_des ~pad:Padding.length key) (marsh x)
	in
		wipe_string key;
		r

(** Hash string [s] given a secret. The resulting hashed filename is human readable. *)
let hash_filename secret s = XString.printable (hash secret s)

(** Generates a random human-readable string of length [len]. *)
let random_printable_string len = 
	let rec f s =
		if String.length s >= len then String.sub s 0 len
		else let s' = s ^ XString.printable (Random.string Random.secure_rng len) in f s'
	in
	    f ""	
				
(**/**)				
let __encode t x = transform_string t (marsh x)
let __decode t s = unmarsh (transform_string t s)
(**/**)	
	
(** Encode [x] in base64. *)
let encode_base64 (x : 'a) : 'a base64 = Base64 (__encode (Base64.encode_compact ()) x)

(** Decode base64 data. *)
let decode_base64 (Base64 s : 'a base64) : 'a = __decode (Base64.decode ()) s
		
(** Compress [x] using the PkZip algorithm. Optionally a [level] different than 9 can be given. *)
let crunch ?(level = 9) (x : 'a) : 'a zip = Zip (__encode (Zlib.compress ~level ()) x)

(** Uncompress zipped data. *)
let decrunch (Zip s : 'a zip) : 'a = __decode (Zlib.uncompress ()) s

(** Encrypt [x] given a secret. The algorithm used is 3DES. *)
let encrypt (Secret f) (x : 'a) : 'a cipher =
	let key = f () in
	let r = Cipher (__encode (Cipher.triple_des ~mode:Cipher.CBC key
				~pad:Padding.length Cipher.Encrypt) x)
	in
		wipe_string key;
		r
		
(** Decrypt encrypted data. The data must have been encrypted with the very same secret. *)		
let decrypt (Secret f) (Cipher s : 'a cipher) : 'a =
	let key = f () in
	let r = __decode (Cipher.triple_des ~mode:Cipher.CBC key
				~pad:Padding.length Cipher.Decrypt) s
	in
		wipe_string key;
		r

(** Authenticate [x] given a secret. *)
let authenticate (Secret f as secret) x =
	let key = f () in
	let auth = hash secret key
	in
		wipe_string key;
		Auth (auth, encrypt (secret_of_string auth) x)
		
(** Unauthenticate authenticated data. The data must have been authenticated with the very same secret. *)
let deauthenticate (Secret f as secret) (Auth (auth, cipher)) =
	let key = f () in
	let auth' = hash secret key
	in
		wipe_string key;
		if auth = auth' then Some (decrypt (secret_of_string auth) cipher)
		else None
		
