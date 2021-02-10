using System;
using System.Linq;

namespace aoc_runner
{
    public record Day05(int[] TakenSeatIds)
    {
        public Day05(string[] input) : this(input.Select(ParseSeatId).ToArray()) { }

        public int Part1() => TakenSeatIds.Max();

        public int Part2()
            => Enumerable.Range(0, TakenSeatIds.Max()) // all possible seats IDs (including empty front rows, excluding last taken one)
                         .Except(TakenSeatIds)         // except the taken seats
                         .Max();                       // my seat is the free seat with the highest id
        // (the others are empty front row seats)

        public static int ParseSeatId(string boardingPass) => Convert.ToInt32(ConvertToBinary(boardingPass), 2);

        public static string ConvertToBinary(string boardingPass) =>
            boardingPass
               .Replace('F', '0')
               .Replace('B', '1')
               .Replace('L', '0')
               .Replace('R', '1');
    }
}