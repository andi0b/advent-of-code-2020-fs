namespace solutions

module Solution03 =

    let isTreeAt (input: string []) (col, row) =
        let inputRow = input.[row]
        inputRow.[col % inputRow.Length] = '#'

    let tobogganPositions (input: string []) (moveRight, moveDown) =
        let totalRowCount = input.Length
        let moveDownCount = totalRowCount / moveDown

        [ 0 .. moveDownCount - 1 ]
        |> Seq.map (fun x -> (moveRight * x, moveDown * x))

    let countTreesOnSlope input moves =
        tobogganPositions input moves
        |> Seq.where (isTreeAt input)
        |> Seq.length

    let part1 input = countTreesOnSlope input (3, 1)

    let part2Movements =
        [ (1, 1)
          (3, 1)
          (5, 1)
          (7, 1)
          (1, 2) ]

    let part2 input =
        part2Movements
        |> Seq.map (countTreesOnSlope input)
        |> Seq.fold (fun x y -> x * int64 y) 1L

type Day03fs(input: string []) =
    member this.Part1() = Solution03.part1 input
    member this.Part2() = Solution03.part2 input
