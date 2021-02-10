using System;
using Xunit;
using FluentAssertions;
using solutions;
using static aoc_runner.Day07;

namespace tests
{
    public class Day07Test
    {
        private static string[] _sampleInput = new[]
        {
            "light red bags contain 1 bright white bag, 2 muted yellow bags.",
            "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
            "bright white bags contain 1 shiny gold bag.",
            "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
            "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
            "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
            "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
            "faded blue bags contain no other bags.",
            "dotted black bags contain no other bags."
        };

        [Fact] Day07fs GetInstance() => new Day07fs(_sampleInput);

        [Fact] void Part1() => GetInstance().Part1().Should().Be(4);

        [Fact] void Part2() => GetInstance().Part2().Should().Be(32);
        
        public static TheoryData<string, BagRule> ParseData = new()
        {
            {"light red bags contain 1 bright white bag, 2 muted yellow bags.",
            new BagRule("light red", new[]{("bright white",1),("muted yellow",2)})},
            
            {"bright white bags contain 1 shiny gold bag.",
            new BagRule("bright white", new[]{("shiny gold",1)})},
            
            {"faded blue bags contain no other bags.",
            new BagRule("faded blue", Array.Empty<(string,int)>())}
        };

        [Theory, MemberData(nameof(ParseData))]
        public void Parse(string input, BagRule expected)
        {
            var parsed = BagRule.Parse(input);
            parsed.Color.Should().Be(expected.Color);
            parsed.Contains.Should().BeEquivalentTo(expected.Contains);
        }
    }
}