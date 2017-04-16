open IJCommon

type t = test

let describe label f =
  Suite (label, f)

let test label f =
  Test (label, f)

let run tests =
  tests
  |> List.map (IJRun.run [])
  |> IJPrint.print_summary

module Skip = struct
  let describe label f =
    SkippedSuite (label, f)

  let test label _ =
    SkippedTest label
end