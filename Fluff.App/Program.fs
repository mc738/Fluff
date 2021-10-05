// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open Fluff.Core
open Fluff.Core.Svg
open Fluff.Core.Svg.Charts

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

let mustacheTest _ =

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

    Mustache.replace d true tokens

let svgTest _ =
    let points =
        { Values =
              [ { X = 5; Y = 10 }
                { X = 10; Y = 40 }
                { X = 40; Y = 30 }
                { X = 60; Y = 5 }
                { X = 90; Y = 45 }
                { X = 120; Y = 10 }
                { X = 150; Y = 45 }
                { X = 200; Y = 10 } ]
              |> Array.ofList
          CurrentIndex = 0 }

    points
    |> createBezierCommand
    |> boilerPlate
    |> (fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test.svg", svg))

let chartTest _ =
    {
        MinValue = 0m
        MaxValue = 1000000m
        Values = [
            500000m
            400000m
            500000m
            600000m
            500000m
            800000m
            700000m
            900000m
            800000m
            600000m
            700000m
            500000m           
        ]
    }.ToChart({ Margin = 20; ViewBoxHeight = 100; ViewBoxWidth = 100 })
    |> (fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_chart.svg", svg))
    
    
    
    
    

[<EntryPoint>]
let main argv =

    chartTest ()
    //svgTest ()
    
    0 // return an integer exit code
