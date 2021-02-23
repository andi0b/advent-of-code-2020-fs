namespace solutions

open System
open System.Text.RegularExpressions

module Solution20 =

    type Hash = int

    type Image =
        { tileId: int
          content: char [,]
          left: Hash
          right: Hash
          top: Hash
          bottom: Hash }

    type Tile =
        { tileId: int
          permutations: Image list }

    let tileSize = 10

    module Image =
        let createHash (chars: char [,]) =
            let shift amount value =
                if value = '#' then (1 <<< amount) else 0

            chars
            |> Seq.cast
            |> Seq.indexed
            |> Seq.fold (fun state (i, v) -> state ||| shift i v) 0

        let create id content =
            { tileId = id
              content = content
              left = createHash content.[0..0, 0..]
              right = createHash content.[tileSize - 1..tileSize - 1, 0..]
              top = createHash content.[0.., 0..0]
              bottom = createHash content.[0.., tileSize - 1..tileSize - 1] }

        // Methods to turn and flip Tiles
        let turn content =
            content
            |> Array2D.mapi (fun x y _ -> content.[tileSize - 1 - y, x])

        let turnMultiple times content =
            [ 1 .. times ]
            |> Seq.fold (fun s _ -> turn s) content

        let flip content =
            content
            |> Array2D.mapi (fun x y _ -> content.[tileSize - 1 - x, y])

        /// Crete all 8 permutations for an image (4 directions + flipped)
        let createPermutations (image: Image) =
            let turnFlip (shouldFlip, turns) =
                if shouldFlip then flip image.content else image.content
                |> turnMultiple turns

            Seq.allPairs [ true; false ] [ 0 .. 3 ]
            |> Seq.map turnFlip
            |> Seq.map (create image.tileId)

        let fitsLeftOf other image = image.right = other.left
        let fitsTopOf other image = image.bottom = other.top

    module Tile =
        let createTile id content =
            let image = Image.create id content

            { tileId = id
              permutations = Image.createPermutations image |> List.ofSeq }

        let parse (str: string) =
            let lines = str.Split(Environment.NewLine)

            let id =
                let m = Regex.Match(lines.[0], @"Tile (\d*):")
                m.Groups.[1].Value |> int

            let readChar x y = lines.[y + 1].[x]
            let content = Array2D.init tileSize tileSize readChar

            createTile id content

    type Sea =
        { tiles: Tile list
          cornerTiles: Image list }

    let createSea (str: string) =
        let tiles =
            str.Split(Environment.NewLine + Environment.NewLine)
            |> Seq.map Tile.parse
            |> Seq.toList

        let allPermutations =
            tiles |> Seq.collect (fun x -> x.permutations)

        let cornerTiles =
            allPermutations
            |> Seq.filter (fun image ->
                let otherPermutations =
                    allPermutations
                    |> Seq.filter (fun other -> other.tileId <> image.tileId)


                let notAnyFitting comparer =
                    otherPermutations |> Seq.exists comparer |> not

                notAnyFitting (Image.fitsLeftOf image)
                && notAnyFitting (Image.fitsTopOf image))
            |> List.ofSeq

        { tiles = tiles
          cornerTiles = cornerTiles }


    let calcCornerTileValue sea =
        sea.cornerTiles
        |> Seq.map (fun x -> int64 x.tileId)
        |> Seq.distinct
        |> Seq.reduce (*)

type Day20fs(input: string) =
    let sea = Solution20.createSea input
    member this.Part1() = Solution20.calcCornerTileValue sea
    member this.Part2() = 1
