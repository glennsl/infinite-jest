module Assertions : sig
  type t
end

module Expect : sig
  val stringContaining : string -> string -> Assertions.t
  val toBeCloseTo : ?digits:int -> float -> float -> Assertions.t
  val toBeFalse : bool -> Assertions.t
  val toBeGreaterThan : int -> int -> Assertions.t
  val toBeLessThan : int -> int -> Assertions.t
  val toBeTrue : bool -> Assertions.t
  val toContain : ?printer:('a -> string) -> 'a -> 'a list -> Assertions.t
  val toContainAll : ?printer:('a -> string) -> 'a list -> 'a list -> Assertions.t
  val toEqual : ?printer:('a -> string) -> 'a -> 'a -> Assertions.t
  val toRaise : (unit -> 'a) -> Assertions.t
end

module Test : sig
  type t

  val describe : string -> (unit -> t list) -> t
  val test : string -> (unit -> Assertions.t) -> t
  val run : t list -> unit

  module Skip : sig
    val describe : string -> (unit -> t list) -> t
    val test : string -> (unit -> Assertions.t) -> t
  end
end