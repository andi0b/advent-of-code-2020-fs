namespace solutions

module Solution01 =

    let part1 numbers =
        List.allPairs numbers numbers
        |> List.find (fun (a, b) -> a + b = 2020)
        |> fun (a, b) -> a * b

    let part2 numbers =
        List.allPairs numbers (List.allPairs numbers numbers) 
        |> List.map (fun (a, (b, c)) -> (a, b, c))
        |> List.find (fun (a, b, c) -> a + b + c = 2020)
        |> fun (a, b, c) -> a * b * c


type Day01fs(input: int []) =
    let numbers = input |> Array.toList
    member this.Part1() = Solution01.part1 numbers
    member this.Part2() = Solution01.part2 numbers
