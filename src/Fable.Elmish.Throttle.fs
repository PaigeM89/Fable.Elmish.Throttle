module Elmish.Throttle

open System
open Elmish

/// Usage:
/// When handling an event (like a mouse move event), call `throttle`, passing in
/// the state (which is `Throttler`), the id of the event being throttled (such as `mouse-event`; all 
/// events being throttled from the same source should have the same name), the time to throttle, and the event itself.
/// If that id is free, then the event is returned, and a message to throttle is also returned
///     That message must be handled by the caller, where it is passed down to this throttler and mapped 
///     to a new state & message.

type Status = 
| Throttled
| Free
type Id = string
type Throttler = Map<Id, Status>
let init() = Map.empty

type Msg =
| Throttle of Id * TimeSpan
| Release of Id
| OnError of exn

// free the id in X time
let freePromise (id : Id) ms =
  promise {
    do! Promise.sleep ms
    return id
  }

// let getReleaseCmd (msg : Msg) mdl =
//   match msg with
//   | Throttle (id, ts) ->
//     let ms = (ts.TotalMilliseconds |> int)
//     let promise = freePromise id
//     let mdl = Map.add id Throttled mdl
//     mdl, Cmd.OfPromise.either promise (ms) Release OnError
//   | _ -> mdl, Cmd.none

let throttle (throttler : Throttler) id (ts : TimeSpan) (ev) =
  match Map.tryFind id throttler with
  | Some Status.Throttled -> None
  | Some Status.Free
  | None ->
    let msg = Throttle (id, ts)
    Some (ev, msg)

let handleThrottleMsg msg mdl =
  match msg with
  | Release id -> (Map.remove id mdl, Cmd.none) |> Ok
  | Throttle (id, ts) ->
    let ms = (ts.TotalMilliseconds |> int)
    let promise = freePromise id
    let mdl = Map.add id Throttled mdl
    (mdl, Cmd.OfPromise.either promise (ms) Release OnError) |> Ok
  | OnError exn -> 
    sprintf "Throttler error: %A" exn |> Error