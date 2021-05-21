module Fable.Elmish.Throttle.Tests

open Fable.Core
open Fable.Core.JsInterop
open Fable.Elmish.Throttle
open Fable.Core.Testing

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

it "Adding works" <| fun () ->
    let expected = 3
    let actual = add 1 2
    Assert.AreEqual(expected,actual)
