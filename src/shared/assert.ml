type t =
| CloseTo : float * float * int -> t
| Equals : 'a * 'a * ('a -> string) option -> t
| False : bool -> t
| GreaterThan : int * int -> t
| LessThan : int * int -> t
| ListContains : 'a list * 'a * ('a -> string) option -> t
| ListContainsAll : 'a list * 'a list * ('a -> string) option -> t
| Raises : (unit -> 'a) -> t
| StringContains : string * string -> t
| True : bool -> t