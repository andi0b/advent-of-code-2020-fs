using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public record Day07(Day07.BagRule[] BagRules) 
    {
        public Day07(string[] input) : this (input.Select(BagRule.Parse).ToArray()){}

        public int Part1()
        {
            var childParentLookup = (
                from rule in BagRules
                from childBag in rule.Contains
                select (childBag, rule)
            ).ToLookup(x => x.childBag.color, x => x.rule.Color);

            return ParentBagColors("shiny gold").Count();

            IEnumerable<string> ParentBagColors(string color) =>
                childParentLookup[color].SelectMany(ParentBagColors)
                                        .Distinct()
                                        .Concat(childParentLookup[color]);
        }

        public int Part2()
        {
            var rulesByColor = BagRules.ToDictionary(x => x.Color, x => x);

            return TotalBagCount("shiny gold") - 1;

            int TotalBagCount(string color)
                => 1 + rulesByColor[color].Contains.Sum(x => x.count * TotalBagCount(x.color));
        }

        public record BagRule(string Color, (string color, int count)[] Contains)
        {
            public static BagRule Parse(string input) => new(
                Color: Regex.Match(input, @"^(\S+ \S+)").Groups[0].Value,
                Contains: (
                    from match in Regex.Matches(input, @"(\d+) (\S+ \S+)")
                    select (match.Groups[2].Value, int.Parse(match.Groups[1].Value))
                ).ToArray()
            );
        }
    }
}