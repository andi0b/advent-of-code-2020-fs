namespace solutions

module Solution01 =
    let findResult (pairs: int array seq) =
        pairs
        |> Seq.find (fun x -> Array.sum x = 2020)
        |> Seq.reduce (*)

    let part1 numbers =
        Seq.allPairs numbers numbers
        |> Seq.map (fun (a, b) -> [| a; b |])
        |> findResult

    let part2 numbers =
        Seq.allPairs numbers numbers
        |> Seq.allPairs numbers
        |> Seq.map (fun (a, (b, c)) -> [| a; b; c |])
        |> findResult

type Day01fs(input: int array) =
    member this.Part1() = Solution01.part1 input
    member this.Part2() = Solution01.part2 input
