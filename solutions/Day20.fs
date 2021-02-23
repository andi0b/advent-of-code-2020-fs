namespace solutions

open System
open System.Text.RegularExpressions

module Solution20 =

    type Tile =
        { tileId: int
          content: char [,]
          left: int
          right: int
          top: int
          bottom: int }

    module Tile =
        let tileSize = 10

        let create id content =
            let max = tileSize - 1

            let hash =
                Seq.cast
                >> Seq.indexed
                >> Seq.fold (fun state (i, v) -> state ||| (if v = '#' then (1 <<< i) else 0)) 0

            { tileId = id
              content = content
              left = hash content.[0..0, 0..]
              right = hash content.[max..max, 0..]
              top = hash content.[0.., 0..0]
              bottom = hash content.[0.., max..max] }

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
        let createPermutations (image: Tile) =
            Seq.allPairs [ flip; id ] [ 0 .. 3 ]
            |> Seq.map (fun (flipMaybe, turns) ->
                image.content
                |> flipMaybe
                |> turnMultiple turns
                |> create image.tileId)

        let fitsLeftOf other image = image.right = other.left

        let fitsTopOf other image = image.bottom = other.top

        let parse (str: string) =
            let lines = str.Split(Environment.NewLine)

            let id =
                let m = Regex.Match(lines.[0], @"Tile (\d*):")
                m.Groups.[1].Value |> int

            let readChar x y = lines.[y + 1].[x]
            let content = Array2D.init tileSize tileSize readChar

            create id content

    type Sea =
        { tiles: Tile list
          cornerTiles: Tile list }

    let createSea (str: string) =
        let tiles =
            str.Split(Environment.NewLine + Environment.NewLine)
            |> Seq.map Tile.parse
            |> Seq.toList

        let permutations =
            tiles
            |> Seq.collect Tile.createPermutations
            |> Seq.toList

        let cornerTiles =
            permutations
            |> Seq.filter (fun tile ->
                let others =
                    permutations
                    |> Seq.filter (fun other -> other.tileId <> tile.tileId)

                let notAny comparer = others |> (not << Seq.exists comparer)

                notAny (Tile.fitsLeftOf tile)
                && notAny (Tile.fitsTopOf tile))
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
