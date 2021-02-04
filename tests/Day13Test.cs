using aoc_runner;
using FluentAssertions;
using Xunit;

namespace tests
{
    public class Day13Test
    {
        [Fact] public Day13 GetInstance() => new Day13("939", "7,13,x,x,59,x,31,19");

        [Fact] public void Part1() => GetInstance().Part1().Should().Be(295);

        [Theory,
         InlineData("7,13,x,x,59,x,31,19", 1068781),
         InlineData("17,x,13,19", 3417),
         InlineData("67,7,59,61", 754018),
         InlineData("67,x,7,59,61", 779210),
         InlineData("67,7,x,59,61", 1261476),
         InlineData("1789,37,47,1889", 1202161486)]
        public void Part2(string input, long expected) =>
            new Day13("", input).Part2(input).Should().Be(expected);
    }
}