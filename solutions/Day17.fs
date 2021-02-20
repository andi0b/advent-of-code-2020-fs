namespace solutions

module Solution17 =

    // A cube location can have a varying amount of dimensions
    type CubeLocation = int list

    // A pocket dimension contains a list of turned on CubeLocations
    type PocketDimension = CubeLocation Set

    let parseCubes (lines: string []): PocketDimension =
        let parseLine (line: string) =
            line
            |> Seq.indexed
            |> Seq.filter (fun (_, x) -> x = '#')
            |> Seq.map fst

        lines
        |> Seq.mapi (fun i line ->
            parseLine line
            |> Seq.map (fun charId -> [ charId; i ]))
        |> Seq.collect id
        |> Set.ofSeq

    let repeat item times =
        seq {
            for _ in 1 .. times do
                yield item
        }

    let addDimensions (dim: PocketDimension) num: PocketDimension =
        let addCoordinates = repeat 0 num |> List.ofSeq
        dim |> Set.map (fun cube -> cube @ addCoordinates)

    let getNeighbours (cube: CubeLocation): CubeLocation list =
        let reducer (x: int list list) (y: int list list) =
            List.allPairs x y
            |> List.map (fun (x, y) -> (x @ y))

        cube
        |> List.map (fun c -> [ [ c - 1 ]; [ c ]; [ c + 1 ] ])
        |> List.reduce reducer
        |> List.where (fun c -> c <> cube)

    let nextIteration (dim: PocketDimension): PocketDimension =
        let isCubeActive = dim.Contains

        let activeNeighborCount =
            getNeighbours
            >> Seq.filter isCubeActive
            >> Seq.length

        let cubesToCheck =
            dim |> Seq.collect getNeighbours |> Set.ofSeq

        let cubeShouldBeActive c =
            match (isCubeActive c, activeNeighborCount c) with
            | (true, 2) -> true
            | (_, 3) -> true
            | _ -> false

        cubesToCheck |> Set.filter cubeShouldBeActive


    let solve dim additionalDimensions =
        let dimension = addDimensions dim additionalDimensions

        [ 1 .. 6 ]
        |> Seq.fold (fun x y -> nextIteration x) dimension
        |> Seq.length


type Day17fs(input: string []) =
    let pocketDimension = Solution17.parseCubes input
    member this.Part1() = Solution17.solve pocketDimension 1
    member this.Part2() = Solution17.solve pocketDimension 2
