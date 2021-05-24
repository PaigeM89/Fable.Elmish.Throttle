module Fable.Elmish.Throttle.Tests

open Fable.Core
open Fable.Core.JsInterop
open Elmish.Throttle
open Fable.Core.Testing

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

let ts = System.TimeSpan.FromMilliseconds 10.

it "returns None when given throttled id" <| fun _ ->
  let input = [ ("key", Status.Throttled)] |> Map.ofList
  let output = throttle input "key" ts ()
  Assert.AreEqual(None, output)

it "returns event when key is different than throttled key" <| fun _ ->
  let input = [ ("key", Status.Throttled)] |> Map.ofList
  let output = throttle input "other-key" ts ()
  Assert.AreEqual(Some((), Throttle("other-key", ts)), output)