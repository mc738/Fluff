namespace Fluff.Razor

open Fluff.Core.Charts
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Rendering

module Internal =

    let buildChart (settings: ChartSettings) (values: Series) (builder: RenderTreeBuilder) =

        let min = settings.Margin
        let maxW = settings.ViewBoxWidth - settings.Margin
        let maxH = settings.ViewBoxHeight - settings.Margin

        let h =
            settings.ViewBoxHeight - (settings.Margin * 2)

        let w =
            settings.ViewBoxWidth - (settings.Margin * 2)

        let stepSize = w / (values.Values.Length - 1)

        //let stepSize = w / (s.Values.Length - 1)


        builder.OpenElement(0, "svg")

        builder.AddAttribute(1, "viewBox", $"0 0 {settings.ViewBoxWidth} {settings.ViewBoxHeight}")
        builder.AddAttribute(2, "version", "1.1")
        builder.AddAttribute(3, "xmlns", "http://www.w3.org/2000/svg")
        builder.AddAttribute(4, "xmlns", "http://www.w3.org/2000/svg")
        builder.AddAttribute(5, "class", "svg")
        builder.OpenElement(6, "path")
        builder.AddAttribute(7, "d", $"M {min} {min} L {min} {maxH}")
        builder.AddAttribute(8, "fill", "none")
        builder.AddAttribute(9, "stroke", "grey")
        builder.AddAttribute(10, "style", "stroke-width: 0.2")


        builder.CloseElement() // Close y axis
        builder.OpenElement(11, "path")
        builder.AddAttribute(12, "d", $"M {min} {maxH} L {maxW - stepSize} {maxH}")
        builder.AddAttribute(13, "fill", "none")
        builder.AddAttribute(14, "stroke", "grey")
        builder.AddAttribute(15, "style", "stroke-width: 0.2")
        builder.CloseElement() // Close x axis

        //<path d="{c}" fill="none" stroke="grey" style="stroke-width: 0.3" />

        //let test = $"""<path d="{c} L {maxW - stepSize} {maxH} L {min} {maxH} Z" fill="rgba(255,0,0,0.1)" stroke="none" style="stroke-width: 0.3" />"""
                    
        builder.OpenElement(16, "path")
        builder.AddAttribute(17, "d", $"{values.ToPoints(settings)}")
        builder.AddAttribute(18, "fill", "none")
        builder.AddAttribute(19, "stroke", "grey")
        builder.AddAttribute(20, "style", "stroke-width: 0.3")
        builder.CloseElement() // Close path

        builder.OpenElement(21, "path")
        builder.AddAttribute(22, "d", $"{values.ToPoints(settings)} L {maxW - stepSize} {maxH} L {min} {maxH} Z")
        builder.AddAttribute(23, "fill", "rgba(255,0,0,0.1)")
        builder.AddAttribute(24, "stroke", "none")
        builder.AddAttribute(25, "style", "stroke-width: 0.3")

        builder.CloseElement() // Close fill
        builder.CloseElement() // Close the svg

        ()

    let createFragment<'a> (value: 'a) (handler: 'a -> RenderTreeBuilder -> unit) = RenderFragment(handler value)

    let renderChart settings values =
        createFragment<Series> values (fun v -> buildChart settings values)

type ChartRenderer() =

    static member Render(settings, values) = Internal.renderChart settings values

    static member RenderTest() =
        { MinValue = 0m
          MaxValue = 1000000m
          Values =
              [ 500000m
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
                500000m ] }
        |> Internal.renderChart (
            { Margin = 20
              ViewBoxHeight = 200
              ViewBoxWidth = 200 }: ChartSettings
        )
