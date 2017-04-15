open Unit
open Unit.Test

let suite =
  describe "my test suite" (fun () -> [
    test "pass" (fun () -> 42 |> Expect.toEqual 42);
    test "fail without printer" (fun () -> true |> Expect.toEqual false);
    test "fail with printer" (fun () -> 23 |> Expect.toEqual ~printer:string_of_int 22);
    Skip.test "skipped fail" (fun () -> true |> Expect.toEqual false);

    describe "nested suite" (fun () -> [
      test "nested pass" (fun () -> 42 |> Expect.toEqual 42);
      test "nested fail" (fun () -> true |> Expect.toEqual false);
      test "exception" (fun () -> raise @@ Invalid_argument "foo");
    ])
  ])
