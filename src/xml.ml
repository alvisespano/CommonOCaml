(*
 * Common library
 * xml.ml: XML and HTML facilities
 *
 * (C) 2007 H-Care srl. Alvise Spanò
 *)

(** Tiny XML library *)

open XString
open Printf

(** The XML Tag type.
Tag is a normal tag with a body.
ATag is a tag with attributes and a body.
QTag is a tag with no body.
QATag is a tag with attributes but no body.
CDATA 
*)
type tag = Tag of string * tag_body
         | ATag of string * (string * string) list * tag_body
         | QTag of string
         | QATag of string * (string * string) list
		 | CDATA of string
		 | Text of string

(** The tag body type. *)
and tag_body = [ `String of string | `Int of int | `Float of float | `Time of float | `Sub of tag list | `Empty ]

(** Pretty prints a list of tags given an optional separator. *)
let rec pretty_tags ?(sep = "") ~tab tags = mappen_strings ~sep (pretty_tag ~tab) tags

(** Pretty prints a tag. *)
and pretty_tag ?(sep = "\n") ?(tab = "") tag =
    let pretty_attrs attrs = mappen_strings ~sep:" " (fun (attr, s) -> sprintf "%s=\"%s\"" attr s) attrs
    and pretty_body = function
          `String s   -> s
        | `Int n      -> string_of_int n
        | `Float x    -> string_of_float x
        | `Time ts    -> Time.pretty_datetime ~date_sep:"/" ~timeofday_sep:"." ~date_notation:`DDMMYYYY ~pretty_weekday:None ts
        | `Sub tags   -> sprintf "\n%s\n%s" (pretty_tags ~sep ~tab:(tab ^ "  ") tags) tab
        | `Empty      -> ""
    in
        match tag with
            Tag (name, body)         -> sprintf "%s<%s>%s</%s>" tab name (pretty_body body) name
          | ATag (name, attrs, body) -> sprintf "%s<%s %s>%s</%s>" tab name (pretty_attrs attrs) (pretty_body body) name
          | QTag name                -> sprintf "%s<%s/>" tab name
          | QATag (name, attrs)      -> sprintf "%s<%s %s/>" tab name (pretty_attrs attrs)
		  | CDATA body				 -> sprintf "%s<![CDATA[%s]]>" tab body
		  | Text s					 -> s


(** Pretty prints an XML file given a [tag] and an optional [doctype]. *)
let pretty_xml ?(sep = "\n") ?doctype tag =
    let doctype = match doctype with Some (name, public, url) -> sprintf "<!DOCTYPE %s PUBLIC \"%s\" \"%s\"" name public url | None -> ""
    in
        sprintf "<?xml version=\"1.0\" encoding=\"utf-8\"?>%s%s%s%s" sep doctype sep (pretty_tag ~sep tag)

(** Pretty prints a list of tags. *)
let pretty_html ?sep tags =
    let root = Tag ("html", `Sub tags)
    in
        pretty_xml ?sep ~doctype:("html", "-//W3C//DTD XHTML 1.0 Strict//EN", "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") root


(** Html shortcuts.
*)
module Html =
  struct
	let tag name l = Tag (name, `Sub l)
	let tag' name s = Tag (name, `String s)
	let stag name s = Tag (name, `String s)
	let t s = Text s
	let p = tag "p"
	let p' = tag' "p"
	let i = tag "i"
	let i' = tag' "i"
	let b = tag "b"
	let b' = tag' "b"
	let html = tag "html"
	let head = tag "head"
	let title s = stag "title" s
	let body = tag "body"
	let h n = stag (sprintf "h%d" n)
	let br = QTag "br"
	let hr = QTag "hr"
	let a ~url l = ATag ("a", ["href", url], `Sub l)
	let a' ~url s = ATag ("a", ["href", url], `String s)
	let ul = tag "ul"
	let li = tag "li"
	let li' = tag' "li"
	let ol = tag "ol"
	let code = tag "code"
	let code' = tag' "code"
	let pre = tag "pre"
	let pre' = tag' "pre"
	let table = tag "table"
	let tr = tag "tr"
	let td l = ATag ("td", ["valign", "top"], `Sub l)
	let td' s = ATag ("td", ["valign", "top"], `String s)
  end
