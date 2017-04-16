module Assert : sig
  type t
end

module Expect : sig
  val stringContaining : string -> string -> Assert.t
  val toBeCloseTo : ?digits:int -> float -> float -> Assert.t
  val toBeFalse : bool -> Assert.t
  val toBeGreaterThan : int -> int -> Assert.t
  val toBeLessThan : int -> int -> Assert.t
  val toBeTrue : bool -> Assert.t
  val toContain : ?printer:('a -> string) -> 'a -> 'a list -> Assert.t
  val toContainAll : ?printer:('a -> string) -> 'a list -> 'a list -> Assert.t
  val toEqual : ?printer:('a -> string) -> 'a -> 'a -> Assert.t
  val toRaise : (unit -> 'a) -> Assert.t
end

module Test : sig
  type t

  val describe : string -> (unit -> t list) -> t
  val test : string -> (unit -> Assert.t) -> t
  val run : t list -> unit

  module Skip : sig
    val describe : string -> (unit -> t list) -> t
    val test : string -> (unit -> Assert.t) -> t
  end
end