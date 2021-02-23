using System.Dynamic;
using System.Linq;
using aoc_runner;
using FluentAssertions;
using solutions;
using Xunit;

namespace tests
{
    public class Day20Test
    {
        [Fact]
        Day20fs GetInstance() => new (_demoInput);

        [Fact]
        void Part1() => GetInstance().Part1().Should().Be(20899048083289);
        
        [Fact]
        void Part2() => GetInstance().Part2().Should().Be(273);

        [Fact]
        void Tile_Parse_ToString() => Day20.Tile.Parse(_tile2311).Format().Should().Be(_tile2311);
        
        [Fact]
        void Tile_Parse_Flip() => Day20.Tile.Parse(_tile2311).Flip().Format().Should().Be(@"Tile 2311:
.#..#.##..
.....#..##
.#..##...#
#...#.####
.###.##.##
###.#...##
##..#.#.#.
..#....#..
.#.#...###
###..###..");

        [Fact]
        void AllPermutations()
        {
            var tile = Day20.Tile.Parse(_tile2311);
            var permutations = tile.AllPermutations();

            permutations.Should().BeEquivalentTo(new[]
            {
                tile,
                tile.TurnClockwise(),
                tile.TurnClockwise().TurnClockwise(),
                tile.TurnClockwise().TurnClockwise().TurnClockwise(),
                tile.Flip(),
                tile.Flip().TurnClockwise(),
                tile.Flip().TurnClockwise().TurnClockwise(),
                tile.Flip().TurnClockwise().TurnClockwise().TurnClockwise(),
            }, o => o.ComparingByMembers<Day20.Tile>());
        }

        [Fact]
        void Tile_Turn_Around()
        {
            var tile = Solution20.TileModule.parse(_tile2311);
            var content = tile.permutations.First().content;
            
            var turnedAround = Solution20.ImageModule.turnMultiple(4, content);
            turnedAround.Should().BeEquivalentTo(content);
        }

        [Fact]
        void Tile_Flip_Ids()
        {
            var image = Solution20.TileModule.parse(_tile2311).permutations.First();
            var flipped = Solution20.ImageModule.create(image.tileId, Solution20.ImageModule.flip(image.content));

            flipped.left.Should().Be(image.right);
            flipped.right.Should().Be(image.left);
        } 

        private string _tile2311 = @"Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###";
        
        private string _demoInput = @"Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...";
    }
}