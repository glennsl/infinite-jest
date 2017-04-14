open Unit
open Unit.Test

let suite =
  describe "my test suite" (fun () -> [
    test "pass" (fun () -> 42 |> Expect.toEqual 42);
    test "fail" (fun () -> true |> Expect.toEqual false)
  ])
