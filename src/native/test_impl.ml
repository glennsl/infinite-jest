open Assert

type t =
| SkippedSuite of string * (unit -> t list)
| SkippedTest of string
| Suite of string * (unit -> t list)
| Test of string * (unit -> Assert.t)

type result =
| Ok of string * float
| Error of string * float
| Skipped of string
| SuiteResult of string * result list

let _string_contains s s' =
  let re = Str.regexp_string s' in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false

type color =
| Red
| Green
| Yellow
| BrightRed
| Blue
| DarkGray
| LightGray

let _print_escape codes =
  print_string @@ "\027[" ^ (codes |> List.map string_of_int |> String.concat ";") ^ "m"

let _with_color color f arg =
  _print_escape 
    begin match color with
    | Red -> [31]
    | Green -> [32]
    | Yellow -> [33]
    | BrightRed -> [31;1]
    | Blue -> [34]
    | DarkGray -> [37;2]
    | LightGray -> [37]
    end;
  f arg;
  _print_escape [0]

let _indent n =
  print_string @@ String.make n ' '

let _print_pseudo_code assertName expected received = begin
  _indent 4;
  _with_color Red print_string received;
  _with_color DarkGray print_string @@ " |> Expect." ^ assertName ^ " ";
  _with_color Green print_endline expected;
end

let _print_expect assertName expected received = begin
  _print_pseudo_code assertName expected received;
  print_newline();
  _indent 4; _with_color LightGray print_endline "Expected:";
  _indent 6; _with_color Green print_endline expected;
  _indent 4; _with_color LightGray print_endline "Received:";
  _indent 6; _with_color Red print_endline received;
end

let _print_assertion_error = function
| CloseTo (a, b, digits) ->
  let expected = string_of_float a in
  let received = string_of_float b in
  let digits = string_of_int digits in

  _indent 4;
  _with_color Red print_string received;
  _with_color DarkGray  print_string " |> Expect.toBeCloseTo ~digits:";
  _with_color Blue print_string digits;
  _with_color Green print_endline expected;
  print_newline();

  _indent 4;
  _with_color LightGray print_string "Expected value to be close to (with";
  _with_color Blue print_string digits;
  _with_color LightGray print_endline "-digit precision):";
  _indent 6; _with_color Green print_endline expected;
  _indent 4; _with_color LightGray print_endline "Received:";
  _indent 6; _with_color Red print_endline received;

| Equals (a, b, maybePrinter) ->
  _print_pseudo_code "toEqual" "<a>" "<b>"

| False _ ->
  _print_expect "toBeFalse" "false" "true"

| GreaterThan (a, b) ->
  let expected = string_of_int a in
  let received = string_of_int b in

  _print_pseudo_code "toBeGreaterThan" expected received;
  print_newline ();
  _indent 4; _with_color LightGray print_endline "Expected value to be greater than:";
  _indent 6; _with_color Green print_endline expected;
  _indent 4; _with_color LightGray print_endline "Received:";
  _indent 6; _with_color Red print_endline received;

| LessThan (a, b) ->
  let expected = string_of_int a in
  let received = string_of_int b in

  _print_pseudo_code "toBeLessThan" expected received;
  print_newline ();
  _indent 4; _with_color LightGray print_endline "Expected value to be less than:";
  _indent 6; _with_color Green print_endline expected;
  _indent 4; _with_color LightGray print_endline "Received:";
  _indent 6; _with_color Red print_endline received;

| ListContains (l, x, maybePrinter) ->
  begin match maybePrinter with
  | Some print ->
    let print_list l = "[" ^ (l |> List.map print |> String.concat ", ") ^ "]" in
    let expected = print x in
    let received = print_list l in

    _print_pseudo_code "toBeLessThan" expected received;
    print_newline ();
    _indent 4; _with_color LightGray print_endline "Expected list:";
    _indent 6; _with_color Red print_endline received;
    _indent 4; _with_color LightGray print_endline "To contain value:";
    _indent 6; _with_color Green print_endline expected;
  | None ->
    _print_pseudo_code "toContain" "[<...>]" "<x>"
  end

| ListContainsAll (l, l', maybePrinter) ->
  begin match maybePrinter with
  | Some print ->
    let print_list l = "[" ^ (l |> List.map print |> String.concat ", ") ^ "]" in
    let expected = print_list l' in
    let received = print_list l in

    _print_pseudo_code "toBeLessThan" expected received;
    print_newline ();
    _indent 4; _with_color LightGray print_endline "Expected list:";
    _indent 6; _with_color Red print_endline received;
    _indent 4; _with_color LightGray print_endline "To contain values:";
    _indent 6; _with_color Green print_endline expected;
  | None ->
    _print_pseudo_code "toContainAll" "[<...>]" "<x>"
  end

| Raises f ->
  _print_pseudo_code "toRaise" "" "function";
  print_newline ();
  _indent 4; _with_color LightGray print_endline "Expected function to raise an expception";

| StringContains (expected, received) ->
  _print_pseudo_code "stringContaining" expected received;
  print_newline ();
  _indent 4; _with_color LightGray print_endline "Expected string:";
  _indent 6; _with_color Red print_endline received;
  _indent 4; _with_color LightGray print_endline "To contain the substring:";
  _indent 6; _with_color Green print_endline expected;

| True _ ->
  _print_expect "toBeTrue" "true" "false"

let _print_error context label assertion = begin
  _indent 2; _with_color BrightRed print_endline
    (label :: context |> List.rev |> String.concat " > ");
  print_newline ();
  _print_assertion_error assertion;
  print_newline ();
  Printexc.print_backtrace stdout;
  print_newline ()
end


let _test = function
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
  begin try
    ignore @@ f ();
    false
  with
    _ -> true
  end
| StringContains (s, s') ->
  _string_contains s s'
| True a ->
  a

let rec _run context = function
| SkippedSuite (label, f) ->
  let skip = function
    | Suite (label, f) -> SkippedSuite (label, f)
    | Test (label, _) -> SkippedTest label
    | t -> t
  in
  SuiteResult (label, f () |> List.map skip |> List.map @@ _run (label :: context))
| SkippedTest label ->
  Skipped label
| Suite (label, f) ->
  SuiteResult (label, f () |> List.map @@ _run (label :: context))
| Test (label, f) ->
  let startTime =
    Sys.time () in
  let time () =
    Sys.time () -. startTime in
  let assertion = f () in
  if _test assertion then
    Ok (label, time ())
  else begin
    _print_error context label assertion;
    Error (label, time ())
  end

let _print_counts results = begin
  let rec count p results =
    results
    |> List.map (function
      | SuiteResult (_, results) -> count p results
      | r when p r -> 1
      | _ -> 0)
    |> List.fold_left (+) 0
  in
  let failed = results |> count (function | Error _ -> true | _ -> false) in
  let skipped = results |> count (function | Skipped _ -> true | _ -> false) in
  let passed = results |> count (function | Ok _ -> true | _ -> false) in
  let total = results |> List.length in
  _with_color BrightRed (Printf.printf "%i failed") failed;
  print_string ", ";
  _with_color Yellow (Printf.printf "%i skipped") skipped;
  print_string ", ";
  _with_color Green (Printf.printf "%i passed") passed;
  Printf.printf ", %i total" total
end

let _print_summary results =
  let print_label label time () =
      Printf.printf "%s (%.0fms)\n" label (time /. 1000.) in 
  let rec print_result level = function
    | Ok (label, time) ->
      _indent level; _with_color Green (print_label label time) ();
    | Error (label, time) ->
      _indent level; _with_color Red (print_label label time) ();
    | Skipped label ->
      assert false (* Should be filtered out *)
    | SuiteResult (label, results) ->
      let non_skipped = results |> List.filter (function | Skipped _ -> false | _ -> true) in
      let count_skipped = List.length results - List.length non_skipped in
      _indent level; _with_color LightGray (Printf.printf "%s:\n") label;
      non_skipped |> List.iter (print_result (level + 2));
      if count_skipped > 0 then begin
        _indent (level + 2); _with_color Yellow print_endline @@ (string_of_int count_skipped) ^ " skipped";
      end
  in
  results |> List.iter (print_result 2);
  print_newline ();
  _print_counts results;
  print_newline ()

let describe label f =
  Suite (label, f)

let test label f =
  Test (label, f)

let run tests =
  tests
  |> List.map (_run [])
  |> _print_summary

module Skip = struct
  let describe label f =
    SkippedSuite (label, f)

  let test label _ =
    SkippedTest label
end