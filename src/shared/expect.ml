open Assertions

let toEqual ?printer b a = Equals (a, b, printer)