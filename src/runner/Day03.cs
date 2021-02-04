using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public class Day03
    {
        private readonly Forest _forest;

        public Day03(string[] input)
            => _forest = Forest.Parse(input);

        public int Part1()
            => TreeCountOnSlope(new(3, 1));

        public long Part2() =>
        (
            from def in new SlopeDefinition[]
            {
                new(1, 1),
                new(3, 1),
                new(5, 1),
                new(7, 1),
                new(1, 2)
            }
            select (long) TreeCountOnSlope(def)
        ).Aggregate((agg, next) => agg * next);
        
        private int TreeCountOnSlope(SlopeDefinition def)
            => TobogganPositions(_forest.RowCount / def.Down, def)
               .Count(pos => _forest[pos.x, pos.y]);

        private IEnumerable<(int x, int y)> TobogganPositions(int count, SlopeDefinition def)
            => from i in Enumerable.Range(0, count)
               select (def.Right * i, def.Down * i);

        public record SlopeDefinition(int Right, int Down);
        
        public record Forest(bool[][] IsTreeMapYx, int PatternWidth)
        {
            public bool this[int x, int y]
                => IsTreeMapYx[y][x % PatternWidth];

            public int RowCount => IsTreeMapYx.Length;
            
            public static Forest Parse(string[] input)
            {
                var isTreeMapYx = (
                    from row in input
                    select (from chr in row
                            select chr == '#').ToArray()
                ).ToArray();

                // we can assume there is at last on row, and all rows are same width
                var patternWidth = isTreeMapYx[0].Length;

                return new Forest(isTreeMapYx, patternWidth);
            }
        }
    }

}