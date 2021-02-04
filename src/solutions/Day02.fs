namespace solutions

open System.Text.RegularExpressions

module Solution02 =


    type Policy = { min: int; max: int; char: char }
    type Input = { password: string; policy: Policy }

    module Input =
        let parse str =
            let m =
                Regex.Match(str, "(\d*)-(\d*) (\w): (.*)")

            { password = m.Groups.[4].Value
              policy =
                  { min = m.Groups.[1].Value |> int
                    max = m.Groups.[2].Value |> int
                    char = m.Groups.[3].Value.[0] } }

        let checkPolicy isValidFunc input = isValidFunc input.policy input.password

        let isValidPart1 policy password =
            let charCount =
                password
                |> Seq.where (fun c -> c = policy.char)
                |> Seq.length

            charCount >= policy.min && charCount <= policy.max

        let isValidPart2 policy (password: string) =
            (password.[policy.min-1] = policy.char)
            <> (password.[policy.max-1] = policy.char)

    let solve inputs isValidFunc =
        inputs
        |> Seq.where (Input.checkPolicy isValidFunc)
        |> Seq.length

type Day02fs(input: string []) =
    let inputs =
        input |> Array.map Solution02.Input.parse

    member this.Part1() = Solution02.solve inputs Solution02.Input.isValidPart1
    member this.Part2() = Solution02.solve inputs Solution02.Input.isValidPart2 
