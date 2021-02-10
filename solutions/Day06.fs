namespace solutions

open System

module Solution06 =

    type Answers = char Set
    type Group = Answers Set

    let parseGroups (input: string): Group [] =
        let parseAnswers = Set.ofSeq

        let parseGroup (g: string) =
            g.Split Environment.NewLine
            |> Array.map parseAnswers
            |> Set.ofArray

        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map parseGroup

    let countAnswers (groupReducer: Group -> Answers) =
        Seq.map (groupReducer >> Seq.length) >> Seq.sum

    let part1 = countAnswers Set.unionMany

    let part2 = countAnswers Set.intersectMany


type Day06fs(input: string) =
    let groups = Solution06.parseGroups input
    member this.Part1() = Solution06.part1 groups
    member this.Part2() = Solution06.part2 groups
