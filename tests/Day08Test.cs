using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day08Test
    {
        private static string[] _demoInput = new[]
        {
            "nop +0",
            "acc +1",
            "jmp +4",
            "acc +3",
            "jmp -3",
            "acc -99",
            "acc +1",
            "jmp -4",
            "acc +6"
        };

        [Fact] Day08fs GetInstance() => new (_demoInput);

        [Fact] void Part1() => GetInstance().Part1().Should().Be(5);
        [Fact] void Part2() => GetInstance().Part2().Should().Be(8);

    }
}