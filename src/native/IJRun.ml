open IJCommon
open IJAssert

let _test_assertion = function
| CloseTo (a, b, digits) ->
  abs_float (a -. b) < (10. ** (-.(float_of_int digits))) /. 2.
| Equals (a, b, _) ->
  a = b
| False a ->
  not a
| GreaterThan (a, b) ->
  a > b
| LessThan (a, b) ->
  a < b
| ListContains (l, x, _) ->
  List.exists ((=) x) l
| ListContainsAll (l, l', _) ->
  l' |> List.for_all @@ fun x -> List.exists ((=) x) l
| Raises f ->
  begin
    try
      ignore @@ f (); false
    with
      _ -> true
  end
| StringContains (s, s') ->
  string_contains s s'
| True a ->
  a

let rec run context = function
| SkippedSuite (label, f) ->
  let skip = function
    | Suite (label, f) -> SkippedSuite (label, f)
    | Test (label, _) -> SkippedTest label
    | t -> t
  in
  SuiteResult (label, f () |> List.map skip |> List.map @@ run (label :: context))
| SkippedTest label ->
  Skipped label
| Suite (label, f) ->
  SuiteResult (label, f () |> List.map @@ run (label :: context))
| Test (label, f) ->
  let startTime = Sys.time () in
  let time () = Sys.time () -. startTime in
  try
    let assertion = f () in
    if _test_assertion assertion then
      Ok (label, time ())
    else begin
      IJPrint.print_error context label assertion;
      Error (label, time ())
    end
  with e ->
    IJPrint.print_exception context label e;
    Error (label, time ())