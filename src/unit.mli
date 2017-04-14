type test
type assertion

val assertEqual : 'a -> 'a -> assertion

val describe : string -> (unit -> test list) -> test
val test : string -> (unit -> assertion) -> test
val run : test list -> unit