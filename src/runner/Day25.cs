using System;
using System.Diagnostics;
using System.Linq;
using MathNet.Numerics.Statistics;

namespace aoc_runner
{
    public record Day25(int[] Input)
    {
        public long Part1() => Transform(Input[1], BruteForceLoopSize(7, Input[0]));

        public static int BruteForceLoopSize(int subjectNumber, int transformationResult)
        {
            var num = 1L;
            for (var loopSize = 1;; loopSize++)
                if (transformationResult == (num = Next(num, subjectNumber)))
                    return loopSize;
        }

        public static long Next(long num, int subjectNumber) => num * subjectNumber % 20201227;

        public static long Transform(int subjectNumber, int loopSize) =>
            Enumerable.Repeat(1, loopSize).Aggregate(1L, (acc, _) => Next(acc, subjectNumber));
    }
}