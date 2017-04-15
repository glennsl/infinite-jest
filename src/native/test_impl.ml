open Assertions

type t =
| Suite of string * (unit -> t list)
| Test of string * (unit -> Assertions.t)

type result =
| Ok of string * float
| Error of string * float * exn

let _string_contains s s' =
  let re = Str.regexp_string s' in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false

exception Assert_error of string

let _fail message =
  raise (Assert_error message)

let _fail_expect expected received =
  _fail @@ "Expected `" ^ expected ^ "`, received `" ^ received ^ "`"

let _assert = function
| CloseTo (a, b, digits) ->
  if abs_float (a -. b) >= (10. ** (-.(float_of_int digits))) /. 2. then
    _fail @@ "Expected value to be close to (with " ^ (string_of_int digits) ^ "-digit precision) `" ^
      (string_of_float a) ^ "`, received `" ^ (string_of_float b) ^ "`"

| Equals (a, b, maybePrinter) ->
  if a != b
  then begin
    match maybePrinter with
    | Some print ->
      _fail_expect (print a) (print b)
    | None ->
      _fail "Expected `a = b`"
  end
| False a ->
  if a then
    _fail_expect "false" "true"
| GreaterThan (a, b) ->
  if a <= b then
    _fail @@ "Expected `" ^ (string_of_int a) ^ " > " ^ (string_of_int b) ^ "`"
| LessThan (a, b) ->
  if a >= b then
    _fail @@ "Expected `" ^ (string_of_int a) ^ " < " ^ (string_of_int b) ^ "`"
| ListContains (l, x, maybePrinter) ->
  if not @@ List.exists ((=) x) l then begin
    match maybePrinter with
    | Some print ->
      let print_list l = "[" ^ (l |> List.map print |> String.concat ", ") ^ "]" in
      _fail @@ "Expected list containing `" ^ (print x) ^ "`, received `" ^ (print_list l) ^ "`"
    | None ->
      _fail "Expected list to contain a specific element"
  end
| ListContainsAll (l, l', maybePrinter) ->
  if not (l' |> List.for_all @@ fun x -> List.exists ((=) x) l) then begin
    match maybePrinter with
    | Some print ->
      let print_list l = "[" ^ (l |> List.map print |> String.concat ", ") ^ "]" in
      _fail @@ "Expected list containing `" ^ (print_list l') ^ "`, received `" ^ (print_list l) ^ "`"
    | None ->
      _fail "Expected list to contain a specific element"
  end
| Raises f -> begin
    try
      ignore @@ f ();
      _fail "Expected function to raise an exception"
    with
    | Assert_error _ as e -> raise e
    | _ -> ()
  end
| StringContains (s, s') -> begin
  if not @@ _string_contains s s' then
    _fail @@ "Expected string containing `" ^ s' ^ "`, received `" ^ s ^ "`"
  end
| True a ->
  if not a then
    _fail_expect "true" "false"

let rec _run context = function
| Suite (name, f) ->
  f () |> List.map @@ _run (name :: context) |> List.flatten
| Test (name, f) ->
  let label =
    List.rev (name :: context) |> String.concat " - " in
  let startTime =
    Sys.time () in
  let time () =
    Sys.time () -. startTime in
  try f () |> _assert;
    [Ok (label, time ())]
  with e ->
    [Error (label, time (), e)]


let describe name f =
  Suite (name, f)

let test name f =
  Test (name, f)

let run tests =
  let results: result list =
    tests |> List.map (_run []) |> List.flatten in
  let count_total =
    results |> List.length in
  let count_ok: int =
    results
    |> List.filter (function | Ok _ -> true | _ -> false)
    |> List.length in
  
  results 
  |> List.iter (function
    | Ok (label, time) ->
      Printf.printf "%f %s" time label;
      print_newline ();
    | Error (label, time, e) ->
      Printf.printf "%f %s: " time label;
      print_string "Test Failure!!!!\n";
      Printexc.to_string e |> print_string;
      print_newline ();
      Printexc.print_backtrace stdout;
      print_newline ();
    );

  Printf.printf "Executed %i tests. %i tests succeeded." count_total count_ok;
  print_newline ()