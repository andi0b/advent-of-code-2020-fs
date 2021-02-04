using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day06(Day06.Group[] Groups)
    {
        public Day06(string input) : this(
            (from g in input.Split(Environment.NewLine + Environment.NewLine)
             select new Group(g.Split(Environment.NewLine))).ToArray()
        ) { }

        public int Part1() => Groups.Sum(g => g.Union.Length);
        public int Part2() => Groups.Sum(g => g.Intersect.Length);
        
        public record Group(string[] CustomsDeclarations)
        {
            public string Intersect => Aggregate(Enumerable.Intersect);
            public string Union => Aggregate(Enumerable.Union);
            private string Aggregate(Func<IEnumerable<char>,IEnumerable<char>, IEnumerable<char>> func) =>
                CustomsDeclarations.Aggregate((agg, next) => new string(func(agg, next).ToArray()));
        }
    }
}