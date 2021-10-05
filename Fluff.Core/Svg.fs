﻿namespace Fluff.Core

open System

module Svg =

    type Point = { X: int; Y: int }

    type Points =
        { Values: Point array
          CurrentIndex: int }

        member p.IsInBounds(i) = i >= 0 && i < p.Values.Length

        member p.Current() = p.Values.[p.CurrentIndex]

        member p.Get(i) =
            match p.IsInBounds i with
            | true -> p.Values.[i] |> Some
            | false -> None

        member p.Next() =
            match p.IsInBounds(p.CurrentIndex + 1) with
            | true -> p.Values.[p.CurrentIndex + 1] |> Some
            | false -> None

        member p.Prev() =
            match p.IsInBounds(p.CurrentIndex - 1) with
            | true -> p.Values.[p.CurrentIndex - 1] |> Some
            | false -> None

        member p.Advance() =
            match p.IsInBounds(p.CurrentIndex + 1) with
            | true ->
                { p with
                      CurrentIndex = p.CurrentIndex + 1 }
                |> Some
            | false -> None

    type Line =
        { Length: double
          Angle: double }

        static member Create(pointA: Point, pointB: Point) =
            let lengthX = pointB.X - pointA.X |> double
            let lengthY = pointB.Y - pointA.Y |> double

            { Length = Math.Sqrt(Math.Pow(lengthX, 2.) + Math.Pow(lengthY, 2.))
              Angle = Math.Atan2(lengthY, lengthX) }

    let createControlPoint (current: Point) (previous: Point option) (next: Point option) (reverse: bool) =
        let pPoint =
            match previous with
            | Some p -> p
            | None -> current

        let nPoint =
            match next with
            | Some p -> p
            | None -> current

        let smoothing = 0.2
        let opLine = Line.Create(pPoint, nPoint)

        let angle =
            match reverse with
            | true -> opLine.Angle + Math.PI
            | false -> opLine.Angle

        let length = opLine.Length * smoothing

        { X = current.X + int (Math.Cos(angle) * length)
          Y = current.Y + int (Math.Sin(angle) * length) }

    let createBezierCommand (points: Points) =
        points.Values
        |> Array.mapi
            (fun i p ->
                match i = 0 with
                | true -> $"M {p.X} {p.Y}"
                | false ->
                    let point = points.Values.[i - 1]

                    let startP =
                        createControlPoint point (points.Get(i - 2)) (Some p) false

                    let endP =
                        createControlPoint p (Some point) (points.Get(i + 1)) true

                    $"C {startP.X} {startP.Y} {endP.X} {endP.Y} {p.X} {p.Y}")
        |> String.concat " "

    let boilerPlate path =
        $"""
    <svg viewBox="0 0 100 100" version="1.1" xmlns="http://www.w3.org/2000/svg" class="svg">
        <path d="M 20 20 L 20 80" fill="none" stroke="grey" />
        <path d="M 20 80 L 80 80" fill="none" stroke="grey" />
        <path d="{path}" fill="none" stroke="grey" />
    </svg>"""

    /// Embed roboto font.
    let defs = """
    <defs>
        <style>
           @font-face {
              font-family: 'Roboto';
              font-style: normal;
              font-weight: 400;
              font-display: swap;
              src: url(data:font/woff2;base64,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) format('woff2');
              unicode-range: U+0000-00FF, U+0131, U+0152-0153, U+02BB-02BC, U+02C6, U+02DA, U+02DC, U+2000-206F, U+2074, U+20AC, U+2122, U+2191, U+2193, U+2212, U+2215, U+FEFF, U+FFFD;
            }
        </style>
    </defs>
    """

    module Charts =

        type ChartSettings =
            { Margin: int
              ViewBoxHeight: int
              ViewBoxWidth: int }

        type Series =
            { MinValue: decimal
              MaxValue: decimal
              Values: decimal list }

            member s.ToPoints(settings: ChartSettings) =
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
                            
                        let extra = match i = 0 with true -> 0 | false -> stepSize / (s.Values.Length - 1)

                        { X = settings.Margin + (stepSize * i) + extra
                          Y = invertedY })
                |> fun p ->
                    { Values = p |> Array.ofList
                      CurrentIndex = 0 }
                |> createBezierCommand
                
                    

            member s.ToChart(settings: ChartSettings) =
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

                    let test = $"""<path d="{c} L {maxW - stepSize} {maxH} L {min} {maxH} Z" fill="rgba(255,0,0,0.1)" stroke="none" style="stroke-width: 0.3" />"""
                    
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