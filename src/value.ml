let rec pp_lid ppf = function
  | Longident.Lident s -> Fmt.string ppf s
  | Ldot (id, s) ->
    pp_lid ppf id; Fmt.char ppf '.'; Fmt.string ppf s
  | Lapply (id1, id2) ->
    Fmt.pf ppf "%a(%a)" pp_lid id1 pp_lid id2

type lid = Longident.t
and +'a t =
  | Char of char
  | String of (int * string)
  | Bytes of (int * bytes)
  | Float of float
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Constr of (lid * 'a t list)
  | Variant of (string * 'a t option)
  | List of 'a t list
  | Array of 'a t list
  | Record of (lid * 'a t) list
  | Tuple of 'a t list
  | Abstract of 'a

class virtual ['a] printer = object (self)

  method virtual abstract : Format.formatter -> 'a -> unit
  method virtual with_paren_abstract : 'a -> bool
  
  method with_paren (x : 'a t) = match x with
    | Constr (_, [])
    | Variant (_, None)
    | List _
    | Array _
    | Record _
    | Tuple _
    | String (_, _)
    | Char _
      -> false
    | Bytes (_, _)
    | Constr (_, _ :: _) 
    | Variant (_, Some _)
        -> true
    | Float f -> f < 0.0 || 1. /. f = neg_infinity
    | Int i -> i < 0
    | Int32 i -> i < 0l
    | Int64 i -> i < 0L
    | Nativeint i -> i < 0n
    | Abstract a -> self#with_paren_abstract a

  method private parens_if pp ppf x =
    if self#with_paren x
    then Fmt.parens pp ppf x
    else pp ppf x
  
  method lid = pp_lid

  method char = Fmt.char
  method int = Fmt.int
  method int32 = Fmt.int32
  method int64 = Fmt.int64
  method nativeint = Fmt.nativeint
  method float = Fmt.float (* TOFIX *)

  method string ppf (maxlen, s) = 
    try
      let len = String.length s in
      let s = if len > maxlen then String.sub s 0 maxlen else s in
      Fmt.pf ppf "%S" s;
      if len > maxlen then
        Fmt.pf ppf "... (* string length %d; truncated *)" len
    with
      Invalid_argument _ (* "String.create" *)-> Fmt.pf ppf "<huge string>"

  method bytes ppf (maxlen, s) =
    Fmt.pf ppf "Bytes.of_string %a"
      self#string (maxlen, Bytes.unsafe_to_string s)

  method constr ppf (lid, arg) = match arg with
    | [] -> self#lid ppf lid
    | [ x ] ->
      Fmt.pf ppf "@[<1>%a@ %a@]" self#lid lid (self#parens_if self#value) x
    | params -> 
      Fmt.pf ppf "@[<1>%a@ %a@]" self#lid lid
        Fmt.(parens @@ list ~sep:(unit ",@ ") self#value) params

  method tag ppf s = Fmt.pf ppf "`%s" s
  method variant ppf (name, arg) = match arg with
    | None -> self#tag ppf name
    | Some arg -> 
      Fmt.pf ppf "@[<1>%a@ %a@]"
        self#tag name (self#parens_if self#value) arg

  method list ppf l =
    Fmt.pf ppf "@[<1>[@ %a]@]" Fmt.(list ~sep:(unit ";@ ") self#value) l
  method array ppf l =
    Fmt.pf ppf "@[<1>[|@ %a|]@]" Fmt.(list ~sep:(unit ";@ ") self#value) l
  method tuple ppf l =
    Fmt.pf ppf "@[<1>(@ %a)@]" Fmt.(list ~sep:(unit ",@ ") self#value) l

  method record ppf r =
    let pp_field ppf (lid, v) =
      Fmt.pf ppf "@[<1>%a@ =@ %a@]" self#lid lid self#value v
    in
    Fmt.pf ppf "@[<1>{%a}@]" Fmt.(list ~sep:(unit ";@ ") pp_field) r
      
  method value ppf : 'a t -> unit = function
    | Char c -> self#char ppf c
    | String s -> self#string ppf s
    | Bytes b -> self#bytes ppf b
    | Float f -> self#float ppf f
    | Int i -> self#int ppf i
    | Int32 i -> self#int32 ppf i
    | Int64 i -> self#int64 ppf i
    | Nativeint i -> self#nativeint ppf i
    | Constr x -> self#constr ppf x
    | Variant x -> self#variant ppf x
    | List x -> self#list ppf x
    | Array x -> self#array ppf x
    | Record x -> self#record ppf x
    | Tuple x -> self#tuple ppf x
    | Abstract x -> self#abstract ppf x

end

module Outcometree = struct
  open Outcometree

  type abstract = [
    | `Stuff of string
    | `Ellipsis
    | `Printer of (Format.formatter -> unit)
  ]

  let rec lid_of_out_ident = function
    | Oide_ident s -> Longident.Lident s
    | Oide_apply (a,b) -> Lapply (lid_of_out_ident a, lid_of_out_ident b)
    | Oide_dot (a,s) -> Ldot (lid_of_out_ident a, s)

  let rec convert :
    Outcometree.out_value -> [> abstract ] t = function
    | Oval_array a -> Array (convert_l a)
    | Oval_char c -> Char c
    | Oval_constr (lid, p) -> Constr (lid_of_out_ident lid, convert_l p)
    | Oval_ellipsis -> Abstract `Ellipsis
    | Oval_float f -> Float f
    | Oval_int i -> Int i
    | Oval_int32 i -> Int32 i
    | Oval_int64 i -> Int64 i
    | Oval_nativeint i -> Nativeint i
    | Oval_list l -> List (convert_l l)
    | Oval_printer p -> Abstract (`Printer p)
    | Oval_record r ->
      Record (List.map (fun (n, v) -> (lid_of_out_ident n, convert v)) r)
    | Oval_string (s, l, Ostr_string) -> String (l, s)
    | Oval_string (s, l, Ostr_bytes) -> Bytes (l, Bytes.unsafe_of_string s)
    | Oval_stuff s -> Abstract (`Stuff s)
    | Oval_tuple t -> Tuple (convert_l t)
    | Oval_variant (name, None) -> Variant (name, None)
    | Oval_variant (name, Some p) -> Variant (name, Some (convert p))
  and convert_l l = List.map convert l

  class pp = object
    inherit [abstract] printer
    method with_paren_abstract = function
      | `Ellipsis | `Stuff _ -> false
      | `Printer _ -> true
    method abstract ppf = function
      | `Ellipsis -> Fmt.string ppf "..."
      | `Stuff s -> Fmt.string ppf s
      | `Printer p -> p ppf
  end

end
