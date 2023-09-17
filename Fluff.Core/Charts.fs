namespace Fluff.Core

open System
open System.IO
open Fluff.Core.Svg
open Microsoft.FSharp.Core

module Charts =

    open Svg

    [<AutoOpen>]
    module Common =

        let roundUp stepAmount i =
            let inverse = 1. / stepAmount
            let dividend = float i * inverse |> Math.Ceiling
            dividend / inverse |> int

        type ValueNormalizer<'T> = NormalizerParameters<'T> -> int

        and NormalizerParameters<'T> = { MaxValue: 'T; Value: 'T }

        //type ChartContext = {
        //    Normalizer:
        //}

        type ValueSeries<'T> = { Items: ValueSeriesItem<'T> list }

        and ValueSeriesItem<'T> =
            { Name: string
              Value: 'T
              Color: string }

    [<RequireQualifiedAccess>]
    module LineChartsOld =

        type Settings =
            { Margin: int
              ViewBoxHeight: int
              ViewBoxWidth: int }

        type Series =
            { MinValue: decimal
              MaxValue: decimal
              Values: decimal list }

            member s.ToPoints(settings: Settings) =
                // Assuming min is 0 for now.

                let diff = s.MaxValue - s.MinValue

                let h =
                    settings.ViewBoxHeight - (settings.Margin * 2)

                let w =
                    settings.ViewBoxWidth - (settings.Margin * 2)

                let stepSize = w / (s.Values.Length - 1)

                s.Values
                |> List.mapi
                    (fun i v ->
                        // Value Percent

                        let invertedY =
                            settings.Margin
                            + int (((diff - v) / diff) * (decimal h))

                        let extra =
                            match i = 0 with
                            | true -> 0
                            | false -> stepSize / (s.Values.Length - 1)

                        { X = settings.Margin + (stepSize * i) + extra
                          Y = invertedY })
                |> fun p ->
                    { Values = p |> Array.ofList
                      CurrentIndex = 0 }
                |> Lines.createBezierCommand

            member s.ToChart(settings: Settings) =
                // Assuming min is 0 for now.

                let diff = s.MaxValue - s.MinValue

                let h =
                    settings.ViewBoxHeight - (settings.Margin * 2)

                let w =
                    settings.ViewBoxWidth - (settings.Margin * 2)

                let stepSize = w / (s.Values.Length - 1)

                s.ToPoints settings
                |> fun c ->
                    let min = settings.Margin
                    let maxW = settings.ViewBoxWidth - settings.Margin
                    let maxH = settings.ViewBoxHeight - settings.Margin

                    let test =
                        $"""<path d="{c} L {maxW - stepSize} {maxH} L {min} {maxH} Z" fill="rgba(255,0,0,0.1)" stroke="none" style="stroke-width: 0.3" />"""

                    let xMarks =
                        s.Values
                        |> List.mapi
                            (fun i _ ->
                                $"""<path d="M {settings.Margin + (stepSize * i)} {maxH + 1} L {settings.Margin + (stepSize * i)} {min}" fill="none" stroke="grey" style="stroke-width: 0.1" />
                                    <text x="{settings.Margin + (stepSize * i)}" y="{maxH + 3}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{i + 1}</text>""")
                        |> String.concat Environment.NewLine

                    let yMarks =
                        let getValueLabel (v: decimal) =
                            match v with
                            | _ when v > 999m && v < 1000000m -> $"£{int v / 1000}k"
                            | _ when v > 999999m -> $"£{int v / 1000000}m"
                            | _ -> $"£{v}"

                        let minLabel = getValueLabel s.MinValue
                        let midLabel = getValueLabel (s.MaxValue / 2m)
                        let maxLabel = getValueLabel s.MaxValue

                        let mid = ((maxH - min) / 2) + settings.Margin

                        $"""
                               <path d="M {min - 1} {maxH} L {min} {maxH}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                               <text x="{min - 2}" y="{maxH}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'; transform: translateY(0.5px);">{minLabel}</text>
                               <path d="M {min - 1} {mid} L {min} {mid}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                               <text x="{min - 2}" y="{mid}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'; transform: translateY(0.5px);">{midLabel}</text>
                               <path d="M {min - 1} {min} L {min} {min}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                               <text x="{min - 2}" y="{min}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'; transform: translateY(0.5px);">{maxLabel}</text>
                            """

                    //[ s.MinValue; s.MaxValue / 2m; s.MaxValue ]
                    //|>

                    $"""<svg viewBox="0 0 {settings.ViewBoxWidth} {settings.ViewBoxHeight}" version="1.1" xmlns="http://www.w3.org/2000/svg" class="svg">
                            {defs}
                            <path d="M {min} {min} L {min} {maxH}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                            <path d="M {min} {maxH} L {maxW - stepSize} {maxH}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                            {xMarks}
                            {yMarks}
                            <path d="{c}" fill="none" stroke="grey" style="stroke-width: 0.3" />
                            {test}
                        </svg>"""

        let createYAxis =
            """<path d="M 0 0 L 100 0" fill="none" stroke="grey" />"""

        let createXAxis =
            """<path d="M 100 0 L 100 100" fill="none" stroke="grey" />"""

    [<RequireQualifiedAccess>]
    module LineCharts =

        type Series<'T> =
            { Normalizer: ValueNormalizer<'T>
              //ToString: 'T -> string
              SplitValueHandler: int -> 'T -> string
              Points: LineChartPoint<'T> list }

        and LineChartPoint<'T> = { Name: string; Value: 'T }

        type Settings =
            { BottomOffset: int
              LeftOffset: int
              TopOffset: int
              RightOffset: int
              Title: string option
              XLabel: string option
              YMajorMarks: int list
              YMinorMarks: int list }

        let createTitle (settings: Settings) (width: int) =
            match settings.Title with
            | Some title ->
                $"""<text x="{settings.LeftOffset + (width / 2)}" y="{5}" style="font-size: 4px; text-anchor: middle; font-family: 'roboto'">{title}</text>"""
            | None -> String.Empty

        let createXMarks (settings: Settings) (series: Series<'T>) (height: int) (barWidth: int) =
            let height = height + settings.TopOffset
    
            series.Points
            |> List.mapi
                (fun i p ->
                    $"""<path d="M {settings.LeftOffset + (barWidth * i)} {height + 1} L {settings.LeftOffset + (barWidth * i)} {height}" fill="none" stroke="grey" style="stroke-width: 0.1" />
                        <text x="{settings.LeftOffset
                                  + (barWidth * i)}" y="{height + 3}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{p.Name}</text>""")
            |> String.concat Environment.NewLine

        let createXLabel (settings: Settings) (height: int) (width: int) =
            match settings.XLabel with
            | Some label ->
                let height = height + settings.TopOffset
                $"""<text x="{settings.LeftOffset + (width / 2)}" y="{height + 6}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{label}</text>"""
            | None -> String.Empty

        let createYMarks (settings: Settings) (height: int) (width: int) (maxValue: 'T) (series: Series<'T>) =
            let major =
                settings.YMajorMarks
                |> List.map
                    (fun m ->
                        let y =
                            float (height + settings.TopOffset)
                            - ((float m / 100.) * float height) // + settings.TopOffset
                        //(float normalizedValue / 100.) * float maxHeight
                        let value = series.SplitValueHandler m maxValue

                        $"""<path d="M {settings.LeftOffset - 1} {y} L {width + settings.LeftOffset} {y}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                            <text x="{8}" y="{y + 0.5}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'">{value}</text>""")
                |> String.concat Environment.NewLine


            let minor =
                settings.YMinorMarks
                |> List.map
                    (fun m ->
                        let y =
                            float (height + settings.TopOffset)
                            - ((float m / 100.) * float height) // + settings.TopOffset
                        //(float normalizedValue / 100.) * float maxHeight
                        let value = series.SplitValueHandler m maxValue

                        $"""<path d="M {settings.LeftOffset - 1} {y} L {width + settings.LeftOffset} {y}" fill="none" stroke="grey" style="stroke-width: 0.1" />
                            <text x="{8}" y="{y + 0.5}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'">{value}</text>""")
                |> String.concat Environment.NewLine

            [ major; minor ]
            |> String.concat Environment.NewLine

        let createYAxis (bottomOffset: int) (leftOffset: int) (height: int) =
            $"""<path d="M {leftOffset} {bottomOffset} L {leftOffset} {bottomOffset + height}" fill="none" stroke="grey" stroke-width="0.2" />"""

        let createXAxis (height: int) (leftOffset: int) (length: int) =
            $"""<path d="M {leftOffset} {height} L {leftOffset + length} {height}" fill="none" stroke="grey" stroke-width="0.2" />"""
        
        let generatePoint
            (startPoint: Point)
            (width: int)
            (maxHeight: int)
            (normalizedValue: int)
            (color: string)
            (valueLabel: string)
            =
            let height =
                (float normalizedValue / 100.) * float maxHeight

            $"""<rect width="{width}" height="{height}" x="{startPoint.X}" y="{float startPoint.Y - height}" fill="{color}" />
                <text x="{float startPoint.X + (float width / 2.)}" y="{float startPoint.Y - height - 2.}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{valueLabel}</text>"""

        let generate (settings: Settings) (series: Series<'T>) (maxValue: 'T) =
            let height =
                100 - settings.TopOffset - settings.BottomOffset

            let width =
                100 - settings.LeftOffset - settings.RightOffset
            // TODO might need to make float
            let pointWidth = width / (series.Points.Length - 1)

            let chart =
                [ createTitle settings width
                  createXAxis 90 10 80
                  createYAxis 10 10 80
                  createXMarks settings series height pointWidth
                  createXLabel settings height width
                  createYMarks settings height width maxValue series ]

            
            series.Points
            |> List.mapi
                (fun i p ->
                    let value =
                        series.Normalizer
                            { MaxValue = maxValue
                              Value = p.Value }

                    let invertedY =
                            settings.BottomOffset
                            + int ((decimal (100 - value) / decimal 100) * (decimal height))
                    
                    { X = settings.LeftOffset + (i * pointWidth)
                      Y = invertedY })
            |> fun p ->
                    { Values = p |> Array.ofList
                      CurrentIndex = 0 }
            |> Lines.createBezierCommand
            |> fun r -> [
                $"""<path d="{r}" fill="none" stroke="grey" style="stroke-width: 0.3" />"""
                $"""<path d="{r} L {100 - settings.RightOffset} {100 - settings.BottomOffset} L {settings.LeftOffset} {100 - settings.BottomOffset} Z" fill="rgba(255,0,0,0.1)" stroke="none" style="stroke-width: 0.3" />"""
            ]
            |> fun r ->
                chart @ r
                |> String.concat Environment.NewLine
                |> boilerPlate true
            |> fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_line_chart.svg", svg)

    [<RequireQualifiedAccess>]
    module PieCharts =

        type Handlers<'T> =
            { Normalizer: ValueNormalizer<'T>
              MaxValue: ValueSeries<'T> -> 'T }

        type Settings =
            { BottomOffset: int
              LeftOffset: int
              TopOffset: int
              RightOffset: int
              Title: string option
              IsDonut: bool }

        //type PTC = { X: int; Y: float  }

        let ptc (center: Point) (radius: float) (angleInDegrees: float) =
            let angleInRadians = (angleInDegrees - 90.) * Math.PI / 180.

            let x =
                center.X
                + (int (radius * Math.Cos(angleInRadians)))

            let y =
                center.Y
                + (int (radius * Math.Sin(angleInRadians)))

            ({ X = x; Y = y }: Point)

        let createSlice (center: Point) (radius: float) (startAngle: float) (endAngle: float) (isDonut: bool) =
            let start = ptc center radius endAngle
            let endP = ptc center radius startAngle

            let largeArchFlag =
                if endAngle - startAngle <= 180 then
                    0
                else
                    1

            match isDonut with
            | true ->
                let innerStart = ptc center (radius / 2.) endAngle
                let innerEnd = ptc center (radius / 2.) startAngle
                $"M {start.X} {start.Y} A {radius} {radius} 0 {largeArchFlag} {0} {endP.X} {endP.Y} M {start.X} {start.Y} L {innerStart.X} {innerStart.Y} A {radius / 2.} {radius / 2.} 0 {largeArchFlag} {0} {innerEnd.X} {innerEnd.Y} L {endP.X} {endP.Y} z"
            | false ->
                $"M {start.X} {start.Y} A {radius} {radius} 0 {largeArchFlag} {0} {endP.X} {endP.Y} L {center.X} {center.Y} z"

        let createPath center radius startAngle endAngle color isDonut =
            let cmd =
                createSlice center radius startAngle endAngle isDonut

            $"<path d=\"{cmd}\" fill=\"{color}\" stroke=\"{color}\" stroke-width=\"0\"></path>"

        let generate (settings: Settings) (handlers: Handlers<'T>) (series: ValueSeries<'T>) =
            // Normalize value (to % basically)
            // multiply by 3.6 to get degrees.

            // Fold over series slices, use last one and current one as points
            let height =
                100 - settings.TopOffset - settings.BottomOffset

            let width =
                100 - settings.LeftOffset - settings.RightOffset

            let center =
                { X = width / 2 + settings.LeftOffset
                  Y = height / 2 + settings.TopOffset }

            let radius = width / 2
            let maxValue = handlers.MaxValue series

            let toDeg (value: int) = float value * 3.6

            series.Items
            |> List.fold
                (fun (acc, prevAngle) vsi ->
                    let value =
                        handlers.Normalizer
                            { MaxValue = maxValue
                              Value = vsi.Value }

                    let newAngle = prevAngle + (toDeg value)

                    printfn $"{value} {prevAngle} {newAngle}"

                    acc
                    @ [ createPath center radius prevAngle newAngle vsi.Color settings.IsDonut ],
                    newAngle)
                ([], 0)
            |> fun (acc, _) ->
                String.concat Environment.NewLine acc
                |> boilerPlate true
            |> fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_pie_chart.svg", svg)

    [<RequireQualifiedAccess>]
    module BarCharts =

        type Bar<'T> =
            { Name: string
              Value: 'T
              Color: string }

        type Series<'T> =
            { Normalizer: ValueNormalizer<'T>
              //ToString: 'T -> string
              SplitValueHandler: int -> 'T -> string
              Bars: Bar<'T> list }

        type Settings =
            { SavePath: string
              BottomOffset: int
              LeftOffset: int
              TopOffset: int
              RightOffset: int
              Title: string option
              XLabel: string option
              YMajorMarks: int list
              YMinorMarks: int list }

        let generateBar
            (startPoint: Point)
            (width: int)
            (maxHeight: int)
            (normalizedValue: int)
            (color: string)
            (valueLabel: string)
            =
            let height =
                (float normalizedValue / 100.) * float maxHeight

            $"""<rect width="{width}" height="{height}" x="{startPoint.X}" y="{float startPoint.Y - height}" fill="{color}" />
                <text x="{float startPoint.X + (float width / 2.)}" y="{float startPoint.Y - height - 2.}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{valueLabel}</text>"""

        let createTitle (settings: Settings) (width: int) =
            match settings.Title with
            | Some title ->
                $"""<text x="{settings.LeftOffset + (width / 2)}" y="{5}" style="font-size: 4px; text-anchor: middle; font-family: 'roboto'">{title}</text>"""
            | None -> String.Empty

        let createXMarks (settings: Settings) (series: Series<'T>) (height: int) (barWidth: int) =
            let height = height + settings.TopOffset

            series.Bars
            |> List.mapi
                (fun i bar ->
                    $"""<path d="M {settings.LeftOffset + (barWidth * i)} {height + 1} L {settings.LeftOffset + (barWidth * i)} {height}" fill="none" stroke="grey" style="stroke-width: 0.1" />
                        <text x="{settings.LeftOffset
                                  + (barWidth * i)
                                  + (barWidth / 2)}" y="{height + 3}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{bar.Name}</text>""")
            |> String.concat Environment.NewLine

        let createXLabel (settings: Settings) (height: int) (width: int) =
            match settings.XLabel with
            | Some label ->
                let height = height + settings.TopOffset
                $"""<text x="{settings.LeftOffset + (width / 2)}" y="{height + 6}" style="font-size: 2px; text-anchor: middle; font-family: 'roboto'">{label}</text>"""
            | None -> String.Empty

        let createYMarks (settings: Settings) (height: int) (width: int) (maxValue: 'T) (series: Series<'T>) =
            let major =
                settings.YMajorMarks
                |> List.map
                    (fun m ->
                        let y =
                            float (height + settings.TopOffset)
                            - ((float m / 100.) * float height) // + settings.TopOffset
                        //(float normalizedValue / 100.) * float maxHeight
                        let value = series.SplitValueHandler m maxValue

                        $"""<path d="M {settings.LeftOffset - 1} {y} L {width + settings.LeftOffset} {y}" fill="none" stroke="grey" style="stroke-width: 0.2" />
                            <text x="{8}" y="{y + 0.5}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'">{value}</text>""")
                |> String.concat Environment.NewLine


            let minor =
                settings.YMinorMarks
                |> List.map
                    (fun m ->
                        let y =
                            float (height + settings.TopOffset)
                            - ((float m / 100.) * float height) // + settings.TopOffset
                        //(float normalizedValue / 100.) * float maxHeight
                        let value = series.SplitValueHandler m maxValue

                        $"""<path d="M {settings.LeftOffset - 1} {y} L {width + settings.LeftOffset} {y}" fill="none" stroke="grey" style="stroke-width: 0.1" />
                            <text x="{8}" y="{y + 0.5}" style="font-size: 2px; text-anchor: end; font-family: 'roboto'">{value}</text>""")
                |> String.concat Environment.NewLine

            [ major; minor ]
            |> String.concat Environment.NewLine

        let createYAxis (bottomOffset: int) (leftOffset: int) (height: int) =
            $"""<path d="M {leftOffset} {bottomOffset} L {leftOffset} {bottomOffset + height}" fill="none" stroke="grey" stroke-width="0.2" />"""

        let createXAxis (height: int) (leftOffset: int) (length: int) =
            $"""<path d="M {leftOffset} {height} L {leftOffset + length} {height}" fill="none" stroke="grey" stroke-width="0.2" />"""

        let generate (settings: Settings) (series: Series<'T>) (maxValue: 'T) =
            let height =
                100 - settings.TopOffset - settings.BottomOffset

            let width =
                100 - settings.LeftOffset - settings.RightOffset
            // TODO might need to make float
            let barWidth = width / series.Bars.Length

            let chart =
                [ createTitle settings width
                  createXAxis 90 10 80
                  createYAxis 10 10 80
                  createXMarks settings series height barWidth
                  createXLabel settings height width
                  createYMarks settings height width maxValue series ]

            series.Bars
            |> List.mapi
                (fun i bar ->
                    let value =
                        series.Normalizer
                            { MaxValue = maxValue
                              Value = bar.Value }

                    { X = settings.LeftOffset + (i * barWidth)
                      Y = settings.BottomOffset + height },
                    value,
                    bar.Color,
                    bar.Value.ToString())
            |> List.map
                (fun (start, value, color, valueLabel) -> generateBar start barWidth height value color valueLabel)
            |> fun r ->
                chart @ r
                |> String.concat Environment.NewLine
                |> boilerPlate true
            |> fun svg -> File.WriteAllText(settings.SavePath, svg)

    [<RequireQualifiedAccess>]
    module FunnelCharts =

        type Settings =
            { BottomOffset: int
              LeftOffset: int
              TopOffset: int
              RightOffset: int
              Title: string option }

        type Series<'T> =
            { Normalizer: ValueNormalizer<'T>
              Stages: Stage<'T> list }

        and Stage<'T> =
            { Name: string
              Value: 'T
              Color: string }

        let generateStagePath (point1: Point) (point2: Point) (point3: Point) (point4: Point) (color: string) =
            // Point 3 and 4 flipped on purpose.
            // P3 is the starting point for the next layer, flipping it here makes other logic simpler.
            $"""<path d="M {point1.X} {point1.Y} L {point2.X} {point2.Y} L  {point4.X} {point4.Y}  L {point3.X} {point3.Y} Z" fill="{color}" stroke="none" /> """

        let generate (settings: Settings) (series: Series<'T>) (maxValue: 'T) =
            let height =
                100 - settings.TopOffset - settings.BottomOffset

            let width =
                100 - settings.LeftOffset - settings.RightOffset

            let stageHeight = height / series.Stages.Length

            series.Stages
            |> List.fold
                (fun (acc, prev1, prev2, i) stage ->
                    let normalizedValue =
                        series.Normalizer
                            { MaxValue = maxValue
                              Value = stage.Value }
                    // The normalized value is the percent of actual that will be filled, so subtract from 100 to get offset.
                    let offset =
                        (float width / 100.)
                        * float (100 - normalizedValue) // 100 - normalizedValue to get amount NOT covered.
                        |> int

                    let p3 =
                        { X = settings.LeftOffset + (offset / 2)
                          Y = i * stageHeight + settings.TopOffset }

                    let p4 =
                        { X = 100 - settings.RightOffset - (offset / 2)
                          Y = i * stageHeight + settings.TopOffset }

                    let newAcc =
                        acc
                        @ [ generateStagePath prev1 prev2 p3 p4 stage.Color ]

                    newAcc, p3, p4, i + 1)
                ([],
                 { X = settings.LeftOffset
                   Y = settings.TopOffset },
                 { X = settings.LeftOffset + width
                   Y = settings.TopOffset },
                 1)
            |> fun (acc, _, _, _) ->
                String.concat Environment.NewLine acc
                |> boilerPlate true
            |> fun svg -> File.WriteAllText("C:\\ProjectData\\TestSvgs\\test_funnel_chart.svg", svg)
