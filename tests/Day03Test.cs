using aoc_runner;
using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day03Test
    {
        private readonly string[] _sampleInput =
        {
            "..##.......",
            "#...#...#..",
            ".#....#..#.",
            "..#.#...#.#",
            ".#...##..#.",
            "..#.##.....",
            ".#.#.#....#",
            ".#........#",
            "#.##...#...",
            "#...##....#",
            ".#..#...#.#",
        };
        
        [Fact] Day03fs GetInstance() => new(_sampleInput);

        [Fact] void Part1() => GetInstance().Part1().Should().Be(7);
        
        [Fact] void Part2() => GetInstance().Part2().Should().Be(336);
    }
}