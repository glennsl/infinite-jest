open Shared

type test

external expect : 'a -> < .. > Js.t = "" [@@bs.val]

let assert_ : assertion -> unit = function
| Equals (a, b) -> (expect a) ## toEqual b

external describe : string -> (unit -> test list) -> test = "" [@@bs.val]

external test : string -> (unit -> unit Js.undefined) -> test = "" [@@bs.val]
let test name f =
  test name (fun () ->
    assert_ @@ f ();
    Js.undefined)

let run _ =
  () (* noop *)