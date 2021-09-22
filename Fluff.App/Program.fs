// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Fluff.Core

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let test1 =
    """Dear {{name}},
Hello!
From {{name2}}
{{! This is {{nested}}}}
{{#person}}
You have and account with id {{accountId}}.

Thank you
{{/person}}
{{^person}}
You have no account.
{{/person}}

"""

[<EntryPoint>]
let main argv =

    let tokens = Mustache.parse test1

    let d =
        ({ Values =
               [ "name", Mustache.Value.Scalar "John Smith"
                 "name2", Mustache.Value.Scalar "Jane Doe"
                 "person",
                 Mustache.Value.Object(
                     [ "accountId", Mustache.Value.Scalar "123-456" ]
                     |> Map.ofList
                 ) ]
               |> Map.ofList
           Partials = Map.empty }: Mustache.Data)

    let r = Mustache.replace d true tokens

    printfn $"{tokens}"
    printfn "******************* Output:"
    printfn $"{r}"

    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code
