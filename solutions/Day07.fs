namespace solutions

open System
open System.Text.RegularExpressions

module Solution07 =

    type Contains = { color: string; count: int }

    type BagRule =
        { color: string
          contains: Contains list }

    module BagRule =
        let parseColor s =
            Regex.Match(s, "^(\S+ \S+)").Groups.[0].Value

        let parseContains s =
            Regex.Matches(s, @"(\d+) (\S+ \S+)")
            |> Seq.map (fun m ->
                { count = m.Groups.[1].Value |> int
                  color = m.Groups.[2].Value })
            |> Seq.toList

        let parse s =
            { color = parseColor s
              contains = parseContains s }

    let parentBagCount color bagRules =
        let reverseLookup =
            seq {
                for rule in bagRules do
                    for childBag in rule.contains do
                        yield (childBag.color, rule.color)
            }
            |> Seq.groupBy fst
            |> Seq.map (fun (k, seq) -> (k, seq |> Seq.map snd))
            |> Map.ofSeq

        let rec parentBagColors bagColor: string seq =
            match reverseLookup |> Map.tryFind bagColor with
            | Some m ->
                m
                |> Seq.collect parentBagColors
                |> Seq.distinct
                |> Seq.append m
            | None -> Seq.empty

        parentBagColors color |> Seq.length


    let totalBagCount color (bagRules: BagRule seq) =

        let rulesByColor =
            bagRules
            |> Seq.map (fun r -> (r.color, r.contains))
            |> Map.ofSeq

        let rec bagCount color =
            1
            + (rulesByColor.[color]
               |> Seq.sumBy (fun x -> x.count * bagCount x.color))

        (bagCount color) - 1


type Day07fs(input: string []) =
    let bagRules =
        input |> Seq.map Solution07.BagRule.parse

    member this.Part1() =
        Solution07.parentBagCount "shiny gold" bagRules

    member this.Part2() =
        Solution07.totalBagCount "shiny gold" bagRules
