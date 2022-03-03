// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open Fluff.Core
open Fluff.Core.Svg
open Fluff.Core.Charts

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
    |> Lines.createBezierCommand
    |> boilerPlate true
    |> (fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test.svg", svg))


(*
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
*)


let pieChartTest _ =
    let center = { X = 50; Y = 50 }
    let radius = 40.

    [ 0., 45., "blue"
      45., 120., "green"
      120., 220., "yellow"
      220., 300., "orange"
      300., 0., "pink" ]
    |> List.map (fun (startAngle, endAngle, color) -> PieCharts.createPath center radius startAngle endAngle color true)
    |> fun r ->
        r
        |> String.concat Environment.NewLine
        |> boilerPlate true
    |> (fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_pie_chart.svg", svg))


let barChartTest _ =
    let settings =
        ({ LeftOffset = 10
           BottomOffset = 10
           TopOffset = 10
           RightOffset = 10
           Title = None
           XLabel = None
           YMajorMarks = []
           YMinorMarks = [] }: BarCharts.Settings)


    let series =
        ({ Normalizer = fun p -> (float  p.Value / float p.MaxValue) * 100. |> int
           SplitValueHandler =
               fun percent maxValue ->
                   (float maxValue / float 100) * float percent
                   |> int
                   |> fun r -> r.ToString()
           Bars =
               [ { Name = "Item 1"
                   Value = 20
                   Color = "red" }
                 { Name = "Item 2"
                   Value = 40
                   Color = "green" }
                 { Name = "Item 3"
                   Value = 30
                   Color = "blue" }
                 { Name = "Item 4"
                   Value = 70
                   Color = "orange" }
                 { Name = "Item 5"
                   Value = 80
                   Color = "pink" } ] }: BarCharts.Series<int>)

    BarCharts.generate settings series 80

(*
    let width = 20
    let maxHeight = 80

    [
        { X = 10; Y = 90 }, 50
        { X = 30; Y = 90 }, 25
        { X = 50; Y = 90 }, 10
        { X = 70; Y = 90 }, 100
    ]
    |> List.map (fun (start, value) -> BarCharts.generateBar start width maxHeight value)
    |> fun r -> r @ [ BarCharts.createXAxis 90 10 80; BarCharts.createYAxis 10 10 80 ] |> String.concat Environment.NewLine |> boilerPlate
    |> (fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_bar_chart.svg", svg))
    *)

[<EntryPoint>]
let main argv =

    pieChartTest ()
    barChartTest ()
    //chartTest ()
    //svgTest ()

    0 // return an integer exit code
