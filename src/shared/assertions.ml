type t =
| Equals : 'a * 'a -> t

let assertEqual a b = Equals (a, b)