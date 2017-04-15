module Assertions : sig
  type t
end

module Expect : sig
  val toEqual : ?printer:('a -> string) -> 'a -> 'a -> Assertions.t
end

module Test : sig
  type t

  val describe : string -> (unit -> t list) -> t
  val test : string -> (unit -> Assertions.t) -> t
  val run : t list -> unit
end