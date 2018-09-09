
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

class virtual ['a] printer : object

  method virtual abstract : 'a Fmt.t
  method virtual with_paren_abstract : 'a -> bool

  method value : 'a t Fmt.t
  method with_paren : 'a t -> bool

  method lid : lid Fmt.t
  method tag : string Fmt.t

  method array : 'a t list Fmt.t
  method bytes : (int * bytes) Fmt.t
  method char : char Fmt.t
  method constr : (lid * 'a t list) Fmt.t
  method float : float Fmt.t
  method int : int Fmt.t
  method int32 : int32 Fmt.t
  method int64 : int64 Fmt.t
  method list : 'a t list Fmt.t
  method nativeint : nativeint Fmt.t
  method record : (lid * 'a t) list Fmt.t
  method string : (int * string) Fmt.t
  method tuple : 'a t list Fmt.t
  method variant : (string * 'a t option) Fmt.t

end

module Outcometree : sig

  type abstract =
    [ `Ellipsis
    | `Printer of Format.formatter -> unit
    | `Stuff of string ]

  val convert : Outcometree.out_value -> [> abstract ] t

  class pp : object
    inherit [abstract] printer
    method abstract : Format.formatter -> abstract -> unit
    method with_paren_abstract : abstract -> bool
  end
  
end
