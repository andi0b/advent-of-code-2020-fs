using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day12Test
    {
        private string[] input =
        {
            "F10",
            "N3",
            "F7",
            "R90",
            "F11",
        };
        
        [Fact] Day12fs GetInstance() => new (input);

        [Fact] void Part1() => GetInstance().Part1().Should().Be(25);
        [Fact] void Part2() => GetInstance().Part2().Should().Be(286);
    }
}