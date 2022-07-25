namespace Fluff.Core

open System.Diagnostics
open System.Text.Json
open System.Web
open Fluff.Core

[<RequireQualifiedAccess>]
module Mustache =

    type Failure =
        | VariableNotFound
        | VariableNotScalar of string

    type Token =
        | Unmodified of string
        | EscapedVariable of string
        | NonEscapedVariable of string
        | SectionStart of string
        | InvertedSectionStart of string
        | SectionEnd of string
        | Comment of string
        | Partial of string
        | SetDelimiter of string * string

        static member Create(input: string) =
            match input.[0] with
            | '#' -> SectionStart(input.[1..].Trim())
            | '/' -> SectionEnd(input.[1..].Trim())
            | '^' -> InvertedSectionStart(input.[1..].Trim())
            | '!' -> Comment(input.[1..].Trim())
            | '>' -> Partial(input.[1..].Trim())
            | '=' ->
                SetDelimiter
                <| (input
                        .Substring(1, input.Length - 2)
                        .Trim()
                        .Split()
                    |> fun r -> r.[0], r.[1])
            | '&' -> NonEscapedVariable(input.[1..].Trim())
            | '{' when input.[input.Length - 1] = '}' -> NonEscapedVariable(input.Substring(1, input.Length - 2).Trim())
            | _ -> EscapedVariable input

    type Value =
        | Scalar of string
        | Object of Map<string, Value>
        | Array of Value list
        | Lambda of Lambda

        static member FromJsonElement(element: JsonElement, inlineRenderer: string -> string) =
            let rec elementToValue (element: JsonElement) =
                match element.ValueKind with
                | JsonValueKind.Array ->
                    element.EnumerateArray()
                    |> List.ofSeq
                    |> List.choose elementToValue
                    |> Value.Array
                    |> Some // None
                | JsonValueKind.False -> Value.Scalar "false" |> Some
                | JsonValueKind.Null -> None
                | JsonValueKind.Number ->
                    element.GetDecimal()
                    |> string
                    |> Value.Scalar
                    |> Some
                | JsonValueKind.Object ->
                    element.EnumerateObject()
                    |> List.ofSeq
                    |> List.choose (fun p ->
                        elementToValue p.Value
                        |> Option.map (fun v -> p.Name, v))
                    |> Map.ofList
                    |> Value.Object
                    |> Some
                | JsonValueKind.String ->
                    element.GetString()
                    |> inlineRenderer
                    |> Value.Scalar
                    |> Some
                | JsonValueKind.True -> Value.Scalar "true" |> Some
                | JsonValueKind.Undefined -> None
                | _ -> None

            elementToValue element

        member v.IsArrayType =
            match v with
            | Array _ -> true
            | _ -> false

        member v.IsObjectType =
            match v with
            | Object _ -> true
            | _ -> false

        member v.IsScalarType =
            match v with
            | Scalar _ -> true
            | _ -> false

        member v.IsLambdaType =
            match v with
            | Lambda _ -> true
            | _ -> false

        member v.GetScalar() =
            match v with
            | Scalar s -> Ok s
            | _ -> Error()

        member v.GetObject() =
            match v with
            | Object o -> Ok o
            | _ -> Error()

        member v.GetArray() =
            match v with
            | Array a -> Ok a
            | _ -> Error()

        member v.GetLambda() =
            match v with
            | Lambda l -> Ok l
            | _ -> Error()

    and Lambda = Map<string, Value> -> Token list -> string

    type Partial = Map<string, Value> -> Token list -> string

    type Data =
        { Values: Map<string, Value>
          Partials: Map<string, Partial> }

        static member FromJson(json: string, inlineRenderer: string -> string) =
            let jDoc = JsonDocument.Parse json

            jDoc.RootElement.EnumerateObject()
            |> List.ofSeq
            |> List.choose (fun jp ->
                Value.FromJsonElement(jp.Value, inlineRenderer)
                |> Option.map (fun v -> jp.Name, v))
            |> Map.ofList
            |> fun r -> ({ Values = r; Partials = Map.empty }: Data)

        member d.TryFind(key) = d.Values.TryFind key

    let rec parser (pi: ParsableInput, tokens: Token list, lastSplit: int) =
        //printfn "%A" pi.CurrentChar

        match pi.InBounds(), pi.Is2Chars('{', '{') with
        | false, _ ->
            //printfn "%i" pi.Position
            tokens
            @ [ pi.GetSlice(lastSplit, pi.Input.Length - 1)
                |> Option.defaultValue ""
                |> Token.Unmodified ]
        | true, true ->
            match pi.NextNonNested() with
            | Some endIndex ->
                //printfn "%i %i" pi.Position endIndex

                let token =
                    pi.GetSlice(pi.Position + 2, endIndex - 1)
                    |> Option.defaultValue ""
                    |> Token.Create

                // Chance to 'remove' (skip) trailing newlines from sections etc.
                // For example:
                //  Hello world!
                //  {{! comment!}}
                //
                //  More text...
                //
                // Would render as
                //  Hello World!
                //
                //
                //  More text...
                //
                // Without this.
                // TODO make configurable.
                let newlineOffSet =
                    match token with
                    | Comment _ ->
                        match pi.GetChar(endIndex + 2), pi.GetChar(endIndex + 3) with
                        | Some c1, Some c2 when c1 = '\r' && c2 = '\n' -> 2
                        | Some c1, _ when c1 = '\n' -> 1
                        | _ -> 0
                    | _ -> 0

                //printfn "%A" slice

                let unmodified =
                    pi.GetSlice(lastSplit, pi.Position - 1)
                    |> Option.defaultValue ""
                    |> Token.Unmodified

                parser (pi.SetPosition(endIndex + 1), tokens @ [ unmodified; token ], endIndex + 2 + newlineOffSet)
            | None -> parser (pi.Advance(1), tokens, lastSplit)
        | true, false -> parser (pi.Advance(1), tokens, lastSplit)


    let parse (input: string) =
        ParsableInput.Create(input, "{{", "}}")
        |> fun pi -> parser (pi, [], 0)


    type Scope =
        { Name: string
          Object: Map<string, Value>
          InnerScope: Scope option }

    type TokenCollection =
        { Name: string
          Inverted: bool
          Tokens: Token list }

        member cs.Add(token) =
            { cs with Tokens = cs.Tokens @ [ token ] }
            |> Mode.Collect

    and Mode =
        | Process
        | Collect of TokenCollection

    let rec replace (data: Data) (lint: bool) (tokens: Token list) =
        tokens
        |> List.fold
            (fun (acc, mode) t ->
                match mode with
                | Process ->
                    match t with
                    | Unmodified s -> (acc @ [ s ], mode)
                    | EscapedVariable s ->
                        match data.TryFind s with
                        | Some v ->
                            match v.GetScalar() with
                            | Ok vs -> (acc @ [ HttpUtility.HtmlEncode vs ], mode)
                            | Error _ -> (acc @ [ "" ], mode)
                        | None -> (acc @ [ "" ], mode)
                    | NonEscapedVariable s ->
                        match data.TryFind s with
                        | Some v ->
                            match v.GetScalar() with
                            | Ok vs -> (acc @ [ vs ], mode)
                            | Error _ -> (acc @ [ "" ], mode)
                        | None -> (acc @ [ "" ], mode)
                    | SectionStart s ->
                        acc,
                        Mode.Collect(
                            { Name = s
                              Inverted = false
                              Tokens = [] }
                        )
                    | InvertedSectionStart s ->
                        acc,
                        Mode.Collect(
                            { Name = s
                              Inverted = true
                              Tokens = [] }
                        )
                    | SectionEnd s -> acc, mode // Don't do anything, this should be handled in collect mode.
                    | Comment c -> acc, mode
                    | Partial p -> failwith "TODO"
                    | SetDelimiter (od, cd) -> failwith "TODO"
                | Collect c ->
                    match t with
                    | SectionEnd s when s = c.Name ->
                        let r =
                            match c.Inverted with
                            | true ->
                                // TODO this should also check for empty lists.
                                data.TryFind s
                                |> Option.bind (fun v ->
                                    match v with
                                    | Value.Array a ->
                                        match a.IsEmpty with
                                        | true -> None
                                        | false -> Some ""
                                    | _ -> Some "")
                                |> Option.defaultWith (fun _ -> replace data lint c.Tokens)
                            | false ->
                                // Process collection.
                                match data.TryFind s with
                                | Some v when v.IsArrayType ->
                                    match v.GetArray() with
                                    | Ok a ->
                                        a
                                        |> List.map (fun v ->
                                            match v.GetObject() with
                                            | Ok o -> replace { Values = o; Partials = data.Partials } lint c.Tokens
                                            | Error _ -> "")
                                        |> String.concat ""
                                    | Error _ -> "" // This should not happen.
                                | Some v when v.IsObjectType ->
                                    match v.GetObject() with
                                    | Ok o -> replace { Values = o; Partials = data.Partials } lint c.Tokens
                                    | Error _ -> "" // This should not happen.
                                | Some v when v.IsLambdaType ->
                                    match v.GetLambda() with
                                    | Ok l -> l data.Values c.Tokens
                                    | Error _ -> ""
                                | _ -> ""

                        (acc @ [ r ], Process)
                    | _ -> (acc, c.Add t))
            ([], Mode.Process)
        |> fun (r, _) -> r
        |> String.concat ""
