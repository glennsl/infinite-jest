open Assertions

type t

external expect : 'a -> < .. > Js.t = "" [@@bs.val]

let assert_ : Assertions.t -> unit = function
| Equals (a, b) -> (expect a) ## toEqual b

external describe : string -> (unit -> t list) -> t = "" [@@bs.val]

external test : string -> (unit -> unit Js.undefined) -> t = "" [@@bs.val]
let test name f =
  test name (fun () ->
    assert_ @@ f ();
    Js.undefined)

let run _ =
  () (* noop *)