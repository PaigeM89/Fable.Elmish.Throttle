module Tests

open Fable.Core.JsInterop

// This is necessary to make webpack collect all test files
importSideEffects "./Fable.Elmish.Throttle.Tests.fs"
