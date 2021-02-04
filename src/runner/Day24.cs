using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day24((int x, int y)[] Moves)
    {
        public Day24(string[] input) : this(input.Select(ParseMoves).ToArray()) {}

        public int Part1() => Moves.GroupBy(m => m)
                                   .Count(m => m.Count() % 2 != 0);

        public int Part2() => Enumerable.Repeat(1, 100).Aggregate(
            (
                from flips in Moves.GroupBy(m => m)
                where flips.Count() % 2 != 0
                select flips.Key
            ).ToHashSet(),

            (blackTiles, _) => (
                from tile in blackTiles.SelectMany(x => AllAdjacent(x).Append(x)).Distinct()
                let isBlack = blackTiles.Contains(tile)
                let adjacentBlackCount = AllAdjacent(tile).Count(blackTiles.Contains)
                where (isBlack, adjacentBlackCount) switch
                {
                    (true, 0 or >2) => false,
                    (false, 2)      => true,
                    _               => isBlack,
                }
                select tile
            ).ToHashSet(),

            blackTiles => blackTiles.Count
        );

        public static (int x, int y) ParseMoves(string input) => (
            from pair in input.Prepend(' ').Zip(input)
            select pair switch
            {
                ('s', 'e') => (+1, -1),
                ('s', 'w') => (+0, -1),
                ('n', 'e') => (+0, +1),
                ('n', 'w') => (-1, +1),
                (_, 'e')   => (+1, +0),
                (_, 'w')   => (-1, +0),
                _          => (+0, +0)
            }
        ).Cast<(int x, int y)>().Aggregate((x: 0, y: 0), (acc, next) => (acc.x + next.x, acc.y + next.y));

        public IEnumerable<(int x, int y)> AllAdjacent((int x, int y) tile) =>
            new (int x, int y)[] {(+1, -1), (+0, -1), (+0, +1), (-1, +1), (+1, +0), (-1, +0)}
               .Select(direction => (tile.x + direction.x, tile.y + direction.y));
    }
}