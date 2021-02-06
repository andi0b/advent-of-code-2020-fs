namespace solutions

open System
open System.Text.RegularExpressions

module Solution04 =

    type Passport = Map<string, string>

    module Passport =
        let parseOne (input: string): Passport =
            Regex.Matches(input, @"(?<key>\S*):(?<value>\S*)\s?")
            |> Seq.map (fun m -> (m.Groups.["key"].Value, m.Groups.["value"].Value))
            |> Map.ofSeq

        let parse (input: string) =
            input.Split(Environment.NewLine + Environment.NewLine)
            |> Array.map parseOne

        let containsNecessaryProperties (passport: Passport) =
            [ "byr"
              "iyr"
              "eyr"
              "hgt"
              "hcl"
              "ecl"
              "pid" ]
            |> Seq.forall passport.ContainsKey

        let tryParseInt (x: string) =
            match Int32.TryParse(x) with
            | true, v -> Some v
            | false, _ -> None

        let isPropertyValid (prop: string * string) =
            match prop with
            | ("byr", byr) -> int byr >= 1920 && int byr <= 2002
            | ("iyr", iyr) -> int iyr >= 2010 && int iyr <= 2020
            | ("eyr", eyr) -> int eyr >= 2020 && int eyr <= 2030
            | ("hgt", hgt) ->
                match (hgt.[hgt.Length - 2..], tryParseInt hgt.[..hgt.Length - 3]) with
                | ("cm", Some cm ) -> cm >= 150 && cm <= 193
                | ("in", Some inch) -> inch >= 59 && inch <= 76
                | _ -> false
            | ("hcl", hcl) ->
                hcl.Length = 7
                && hcl.[0] = '#'
                && hcl.[1..]
                   |> Seq.forall (fun c -> Char.IsDigit(c) || (c >= 'a' && c <= 'f'))
            | ("pid", pid) -> pid.Length = 9 && pid |> Seq.forall Char.IsDigit
            | ("cid", _) -> true
            | ("ecl",
               ("amb"
               | "blu"
               | "brn"
               | "gry"
               | "grn"
               | "hzl"
               | "oth")) -> true
            | _ -> false

        let isValid (passport: Passport) =
            passport
            |> Map.toList
            |> Seq.forall isPropertyValid


    let part1 passports =
        passports
        |> Seq.where Passport.containsNecessaryProperties
        |> Seq.length

    let part2 passports =
        passports
        |> Seq.where Passport.containsNecessaryProperties
        |> Seq.where Passport.isValid
        |> Seq.length


type Day04fs(input: string) =
    let passports = Solution04.Passport.parse input
    member this.Part1() = Solution04.part1 passports
    member this.Part2() = Solution04.part2 passports
