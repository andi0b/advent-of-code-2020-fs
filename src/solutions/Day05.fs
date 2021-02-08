namespace solutions

open System

module Solution05 =

    module seatId =
        let convertToBinaryString =
            String.map (function
                | 'B'
                | 'R' -> '1'
                | _ -> '0')

        let intBin s = Convert.ToInt32(s, 2)

        let parse = intBin << convertToBinaryString

    let parseSeatIds = Seq.map seatId.parse

    let highestSeatId = Seq.max

    let findLastFreeSeat takenSeats =
        let seatMap = Set.ofSeq takenSeats

        [ 0 .. Seq.max takenSeats ]
        |> Seq.findBack (not << seatMap.Contains)


type Day05fs(input: string array) =
    let takenSeats = Solution05.parseSeatIds input
    member this.Part1() = Solution05.highestSeatId takenSeats
    member this.Part2() = Solution05.findLastFreeSeat takenSeats
