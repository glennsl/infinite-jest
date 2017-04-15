open Assertions

let stringContaining b a = StringContains (a, b)
let toBeCloseTo ?(digits=2) b a = CloseTo (a, b, digits)
let toBeFalse a = False a
let toBeGreaterThan b a = GreaterThan (a, b)
let toBeLessThan b a = LessThan (a, b)
let toBeTrue a = True a
let toContain ?printer item list = ListContains (list, item, printer)
let toContainAll ?printer subset list = ListContainsAll (list, subset, printer)
let toEqual ?printer b a = Equals (a, b, printer)
let toRaise f = Raises f