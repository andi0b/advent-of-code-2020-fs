using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day06Test
    {
        private string demoInput = @"abc

a
b
c

ab
ac

a
a
a
a

b";

        [Fact] public Day06fs GetInstance() => new (demoInput);
        [Fact] void Part1() =>  GetInstance().Part1().Should().Be(11);
        [Fact] void Part2() =>  GetInstance().Part2().Should().Be(6);
    }
}