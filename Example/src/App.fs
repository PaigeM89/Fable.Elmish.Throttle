module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Elmish.Throttle

open System

type Model = {
    Clicks : int
    ThrottleState: Map<string, Throttle.Status>
}

type Msg = 
| IncrementClick
| ThrottleMsg of Throttle.Msg



let throttleOnClick model ev =
    throttle model.ThrottleState "button-click" (TimeSpan.FromSeconds 1.) ev

let view model dispatch =
    div [] [
        h1 [] [ str "Hello world!" ]
        button [ OnClick (fun ev ->
            match throttleOnClick model ev with
            | None -> ()
            | Some (ev, throttleMsg) ->
                dispatch (ThrottleMsg throttleMsg)
                IncrementClick |> dispatch
            )
        ] [ str "Click me!" ]
        h2 [] [ str (sprintf "button has been clicked %i times!" model.Clicks)]
    ]

let update msg model : (Model * Cmd<Msg>)=
    match msg with
    | IncrementClick -> { model with Clicks = model.Clicks + 1}, Cmd.none
    | ThrottleMsg throttleMsg ->
        let throttleResult = handleThrottleMsg throttleMsg model.ThrottleState
        match throttleResult with
        | Ok (throttleState, throttleCmd) ->
            { model with ThrottleState = throttleState }, Cmd.map ThrottleMsg throttleCmd
        | Error e ->
            printfn "Error throttling: %A" e
            model, Cmd.none


let init() =
    { Clicks = 0; ThrottleState = Map.empty }, Cmd.none

Program.mkProgram
    init
    update
    view
|> Program.withReactBatched "root"
|> Program.run