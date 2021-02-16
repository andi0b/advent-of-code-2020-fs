namespace solutions

open System.Text.RegularExpressions

module Solution08 =

    type Operation =
        | Acc of int
        | Jmp of int
        | Nop of int

    type BootCode = Operation list

    let parseOp s: Operation =
        let m = Regex.Match(s, "(?<op>\S*) (?<arg>\S*)")
        let op = m.Groups.["op"].Value
        let arg = m.Groups.["arg"].Value |> int

        match op with
        | "acc" -> Acc arg
        | "jmp" -> Jmp arg
        | "nop" -> Nop arg
        | _ -> failwith "unknown operation"

    let parseBootCode l: BootCode = l |> Seq.map parseOp |> List.ofSeq

    module Handheld =
        type HandheldState =
            { accumulator: int
              instructionId: int }

        let initialState = { accumulator = 0; instructionId = 0 }

        let nextState (code: BootCode) (state: HandheldState) =
            match code.[state.instructionId] with
            | Acc param ->
                { accumulator = state.accumulator + param
                  instructionId = state.instructionId + 1 }
            | Jmp param ->
                { state with
                      instructionId = state.instructionId + param }
            | Nop _ ->
                { state with
                      instructionId = state.instructionId + 1 }

        let isFinished (code: BootCode) (state: HandheldState) = state.instructionId = code.Length

        let extractFinishedState code state =
            {| accumulatorEndState = state.accumulator
               isFinished = isFinished code state |}

        let run code =
            let generator (instructionIdHistory: Set<int>, state) =

                if isFinished code state then
                    None
                else
                    let nextState = nextState code state

                    let isInfiniteLoop =
                        instructionIdHistory.Contains nextState.instructionId

                    if isInfiniteLoop
                    then None
                    else Some(nextState, (instructionIdHistory.Add nextState.instructionId, nextState))

            Seq.unfold generator (Set.empty, initialState)
            |> Seq.last
            |> extractFinishedState code


    let getLastAccumulator code = (Handheld.run code).accumulatorEndState

    let createCodePermutations (code: BootCode): BootCode list =

        let flipNopJmp =
            function
            | Jmp arg -> Some(Nop arg)
            | Nop arg -> Some(Jmp arg)
            | _ -> None

        let replaceAt idx value =
            List.indexed
            >> List.map (fun (i, x) -> if i = idx then value else x)

        code
        |> List.indexed
        |> List.choose (fun (i, op) ->
            match (flipNopJmp op) with
            | Some flippedOp -> Some(code |> (replaceAt i flippedOp))
            | None -> None)

    let getLastAccumulatorForFixedCode code =
        createCodePermutations code
        |> Seq.map Handheld.run
        |> Seq.find (fun x -> x.isFinished)
        |> fun x -> x.accumulatorEndState


type Day08fs(input: string []) =
    let bootCode = Solution08.parseBootCode input

    member this.Part1() = Solution08.getLastAccumulator bootCode

    member this.Part2() =
        Solution08.getLastAccumulatorForFixedCode bootCode
