namespace solutions

open System

module Solution06 =

    type Answers = char Set
    type Group = Answers array

    let parseGroups (input: string): Group [] =
        let parseAnswers = Set.ofSeq

        let parseGroup (g: string) =
            g.Split Environment.NewLine
            |> Array.map parseAnswers

        input.Split(Environment.NewLine + Environment.NewLine)
        |> Array.map parseGroup

    let countAnswers (groupReducer: Group -> Answers) =
        Seq.sumBy (groupReducer >> Seq.length) 

    let part1 = countAnswers Set.unionMany

    let part2 = countAnswers Set.intersectMany


type Day06fs(input: string) =
    let groups = Solution06.parseGroups input
    member this.Part1() = Solution06.part1 groups
    member this.Part2() = Solution06.part2 groups
