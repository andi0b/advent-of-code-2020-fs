namespace solutions

open System.Text.RegularExpressions

module Solution08 =

    type Operation =
        | Acc of int
        | Jmp of int
        | Nop of int
        static member parse s: Operation =
            let m = Regex.Match(s, "(?<op>\S*) (?<arg>\S*)")
            let op = m.Groups.["op"].Value
            let arg = m.Groups.["arg"].Value |> int

            match op with
            | "acc" -> Acc arg
            | "jmp" -> Jmp arg
            | "nop" -> Nop arg
            | _ -> failwith "unknown operation"

    type BootCode = Operation list

    let parse l: BootCode =
        l |> Seq.map Operation.parse |> List.ofSeq

    type HandheldState =
        { accumulator: int
          nextInstructionId: int
          isFinished: bool
          isInfiniteLoop: bool }
        static member Initial =
            { accumulator = 0
              nextInstructionId = 0
              isFinished = false
              isInfiniteLoop = false }

    let nextState (code: BootCode) (opHistory: Set<int>) (state: HandheldState) =

        let applyOp s =
            match code.[s.nextInstructionId] with
            | Acc param ->
                { s with
                      accumulator = s.accumulator + param
                      nextInstructionId = s.nextInstructionId + 1 }
            | Jmp param ->
                { s with
                      nextInstructionId = s.nextInstructionId + param }
            | Nop _ ->
                { s with
                      nextInstructionId = s.nextInstructionId + 1 }

        let checkFinished s =
            { s with
                  isFinished = s.nextInstructionId = code.Length }

        let checkInfiniteLoop s =
            { s with
                  isInfiniteLoop = opHistory.Contains(s.nextInstructionId) }

        state
        |> applyOp
        |> checkFinished
        |> checkInfiniteLoop


    let run code =

        let generator (opHistory, handheldState) =
            if handheldState.isFinished then
                None
            else
                match nextState code opHistory handheldState with
                | { isInfiniteLoop = true } -> None
                | nxt -> Some(nxt, (opHistory.Add nxt.nextInstructionId, nxt))

        Seq.unfold generator (Set.empty, HandheldState.Initial)
        |> Seq.last

    let getLastAccumulator code = (run code).accumulator

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
        |> Seq.map run
        |> Seq.find (fun x -> x.isFinished)
        |> fun x -> x.accumulator


type Day08fs(input: string []) =
    let bootCode = Solution08.parse input

    member this.Part1() = Solution08.getLastAccumulator bootCode

    member this.Part2() =
        Solution08.getLastAccumulatorForFixedCode bootCode
