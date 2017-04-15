open Assertions

type t =
| Suite of string * (unit -> t list)
| Test of string * (unit -> Assertions.t)

type result =
| Ok of string * float
| Error of string * float * exn


let _assert = function
| Equals (a, b, maybePrinter) ->
  if a != b
  then match maybePrinter with
  | Some print ->
    failwith ("Expected `" ^ (print b) ^ "`, received `" ^ (print a) ^ "`")
  | None ->
    failwith "a != b"

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
  with | e ->
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