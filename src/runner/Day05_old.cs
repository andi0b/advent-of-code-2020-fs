using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public class Day05_old
    {
        private readonly BoardingPass[] _boardingPasses;
        public Day05_old(string[] input) => _boardingPasses = input.Select(BoardingPass.Parse).ToArray();
        public int Part1() =>
            _boardingPasses.Max(x => x.SeatId);

        public int Part2()
        {
            var seatplan = new bool[128, 8];
            foreach (var bp in _boardingPasses)
                seatplan[bp.Row, bp.Column] = true;

            var mySeat =
                (
                    from row in Enumerable.Range(0, 128)
                    from column in Enumerable.Range(0, 8)
                    select (row, column, istaken: seatplan[row, column])
                )
               .SkipWhile(x => !x.istaken)
               .First(x => !x.istaken);

            return new BoardingPass(mySeat.row, mySeat.column).SeatId;
        }
            
        
        public record BoardingPass(int Row, int Column)
        {
            public int SeatId => 8 * Row + Column;

            public static BoardingPass Parse(string input) =>
                new(ParseBinary(input[..7]), ParseBinary(input[7..]));

            static int ParseBinary(string str) =>
                    Convert.ToInt32(str.Replace('F', '0').Replace('B', '1').Replace('R', '1').Replace('L', '0'), 2);
        }
    }
}