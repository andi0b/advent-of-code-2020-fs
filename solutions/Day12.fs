namespace solutions

open System

module Solution12 =

    type Heading =
        | North = 0
        | East = 1
        | South = 2
        | West = 3

    type TurnInstruction =
        | Left of int
        | Right of int

    type Instruction =
        | Move of Heading * int
        | MoveForward of int
        | Turn of TurnInstruction

    let parseInstruction (s: string): Instruction =
        match (s.[0..0], s.[1..] |> int) with
        | ("L", degrees) -> Turn(Left degrees)
        | ("R", degrees) -> Turn(Right degrees)
        | ("N", distance) -> Move(Heading.North, distance)
        | ("E", distance) -> Move(Heading.East, distance)
        | ("S", distance) -> Move(Heading.South, distance)
        | ("W", distance) -> Move(Heading.West, distance)
        | ("F", distance) -> MoveForward distance
        | _ -> failwith "unknown instruction"

    type Vector =
        { x: int
          y: int }
        static member Initial = { x = 0; y = 0 }
        member this.manhattanDistanceFromZero = abs this.x + abs this.y
        static member (+)(a, b: Vector) = { x = a.x + b.x; y = a.y + b.y }
        static member (*)(a: Vector, b: int) = { x = a.x * b; y = a.y * b }

        member this.move heading distance =
            let moveVector =
                match heading with
                | Heading.North -> { x = 0; y = distance }
                | Heading.East -> { x = distance; y = 0 }
                | Heading.South -> { x = 0; y = (-distance) }
                | Heading.West -> { x = (-distance); y = 0 }
                | _ -> ArgumentOutOfRangeException() |> raise

            this + moveVector


    let turnHeading (oldHeading: Heading) (turnInstruction: TurnInstruction): Heading =
        let quarterRightTurns =
            match turnInstruction with
            | Right deg -> deg / 90
            | Left deg -> -deg / 90

        (4
         + LanguagePrimitives.EnumToValue oldHeading
         + (quarterRightTurns % 4)) % 4
        |> LanguagePrimitives.EnumOfValue

    type Ship =
        { location: Vector
          heading: Heading }
        static member Initial =
            { location = Vector.Initial
              heading = Heading.East }


    let part1 i =

        let move ship =
            let moveHelper h d =
                { ship with
                      location = ship.location.move h d }

            function
            | Turn ti ->
                { ship with
                      heading = turnHeading ship.heading ti }
            | Move (heading, distance) -> moveHelper heading distance
            | MoveForward distance -> moveHelper ship.heading distance


        (i |> Seq.fold move Ship.Initial)
            .location.manhattanDistanceFromZero

    type Ship2 =
        { ship: Vector
          waypoint: Vector }
        static member Initial =
            { ship = Vector.Initial
              waypoint = { x = 10; y = 1 } }

    let part2 i =
        let move state =
            function
            | Turn ti ->
                let newWaypoint =
                    let x = state.waypoint.x
                    let y = state.waypoint.y

                    match turnHeading Heading.North ti with
                    | Heading.East -> { x = y; y = (-x) }
                    | Heading.West -> { x = (-y); y = x }
                    | Heading.South -> { x = (-x); y = (-y) }
                    | _ -> state.waypoint

                { state with waypoint = newWaypoint }

            | Move (heading, distance) ->
                { state with
                      waypoint = state.waypoint.move heading distance }

            | MoveForward distance ->
                { state with
                      ship = state.ship + (state.waypoint * distance) }

        (i |> Seq.fold move Ship2.Initial)
            .ship.manhattanDistanceFromZero

type Day12fs(input: string []) =
    let instructions =
        input |> Array.map Solution12.parseInstruction

    member this.Part1() = Solution12.part1 instructions
    member this.Part2() = Solution12.part2 instructions
