namespace solutions

open System

module Solution05 =

    let convertToBinaryString =
        String.map (function
            | 'B'
            | 'R' -> '1'
            | _ -> '0')

    let intFromBinary binaryString = Convert.ToInt32(binaryString, 2)
    
    let parseSeatIds =
        Seq.map (intFromBinary << convertToBinaryString)

    let highestSeatId = Seq.max

    let findLastFreeSeat takenSeats =
        let seatMap = Set.ofSeq takenSeats

        [ 0 .. Seq.max takenSeats ]
        |> Seq.findBack (not << seatMap.Contains)



type Day05fs(input: string array) =
    let takenSeats = Solution05.parseSeatIds input
    member this.Part1() = Solution05.highestSeatId takenSeats
    member this.Part2() = Solution05.findLastFreeSeat takenSeats
