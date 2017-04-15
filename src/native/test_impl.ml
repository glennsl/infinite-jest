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

let _print_error context name assertion = begin
  _indent 2; _with_color BrightRed print_endline
    (name :: context |> List.rev |> String.concat " > ");
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
| SkippedSuite (name, f) ->
  f ()
  |> List.map (function
    | Suite (name, f) -> SkippedSuite (name, f)
    | Test (name, _) -> SkippedTest name
    | t -> t)
  |> List.map @@ _run (name :: context)
  |> List.flatten
| SkippedTest name ->
  let label =
    List.rev (name :: context) |> String.concat " - " in
  [Skipped label]
| Suite (name, f) ->
  f () |> List.map @@ _run (name :: context) |> List.flatten
| Test (name, f) ->
  let label =
    List.rev (name :: context) |> String.concat " - " in
  let startTime =
    Sys.time () in
  let time () =
    Sys.time () -. startTime in
  let assertion = f () in
  if _test assertion then
    [Ok (label, time ())]
  else begin
    _print_error context name assertion;
    [Error (label, time ())]
  end

let _print_summary results =
  let count_total =
    results |> List.length in
  let count_ok =
    results
    |> List.filter (function | Ok _ -> true | _ -> false)
    |> List.length in
  let count_skipped =
    results
    |> List.filter (function | Skipped _ -> true | _ -> false)
    |> List.length in

  results 
  |> List.iter (function
    | Ok (label, time) ->
      _with_color Green (fun () -> Printf.printf "%f %s" time label) ();
      print_newline ();
    | Error (label, time) ->
      _with_color Red (fun () -> Printf.printf "%f %s: " time label) ();
      print_newline ();
    | Skipped label ->
      _with_color Yellow (Printf.printf "skipped %s") label;
      print_newline ();
    );

  Printf.printf "Executed %i tests. %i tests succeeded. %i skipped" count_total count_ok count_skipped;
  print_newline ()

let describe name f =
  Suite (name, f)

let test name f =
  Test (name, f)

let run tests =
  tests
  |> List.map (_run [])
  |> List.flatten
  |> _print_summary

module Skip = struct
  let describe name f =
    SkippedSuite (name, f)

  let test name _ =
    SkippedTest name
end