open Unit.Assertions
open Unit.Test

let suite =
  describe "my test suite" (fun () -> [
    test "pass" (fun () -> assertEqual 42 42);
    test "fail" (fun () -> assertEqual true false)
  ])
