open Unit
open Unit.Test

let suite =
  describe "my test suite" (fun () -> [
    test "pass" (fun () -> 42 |> Expect.toEqual 42);
    test "fail wiuthout printer" (fun () -> true |> Expect.toEqual false);
    test "fail with printer" (fun () -> 23 |> Expect.toEqual ~printer:string_of_int 22);
  ])
