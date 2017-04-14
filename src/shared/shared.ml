type assertion =
| Equals : 'a * 'a -> assertion

let assertEqual a b = Equals (a, b)