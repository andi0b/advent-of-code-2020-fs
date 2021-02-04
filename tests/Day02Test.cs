using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day02Test
    {
        private readonly string[] _sampleInput =
        {
            "1-3 a: abcde",
            "1-3 b: cdefg",
            "2-9 c: ccccccccc",
        };
        
        [Fact] Day02fs GetInstance() => new(_sampleInput);
        [Fact] void Part1() => GetInstance().Part1().Should().Be(2);
        [Fact] void Part2() => GetInstance().Part2().Should().Be(1);

        [Theory,
         InlineData("1-3 a: abcde", "abcde", 'a', 1, 3),
         InlineData("1-3 b: cdefg", "cdefg", 'b', 1, 3),
         InlineData("2-9 c: ccccccccc", "ccccccccc", 'c', 2, 9)
        ]
        void Parse(string input, string password, char policyChar, int min, int max)
            => Solution02.InputModule.parse(input).Should()
                         .BeEquivalentTo((password, new Solution02.Input(password, new Solution02.Policy(min, max, policyChar))));
    }
}