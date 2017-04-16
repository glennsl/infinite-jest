open IJAssert

type test =
| SkippedSuite of string * (unit -> test list)
| SkippedTest of string
| Suite of string * (unit -> test list)
| Test of string * (unit -> IJAssert.t)

type result =
| Ok of string * float
| Error of string * float
| Skipped of string
| SuiteResult of string * result list

let string_contains s s' =
  let re = Str.regexp_string s' in
  try
    ignore (Str.search_forward re s 0);
    true
  with Not_found ->
    false
