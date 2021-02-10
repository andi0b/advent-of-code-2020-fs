using System;
using System.Linq;

namespace aoc_runner
{
    public record Day09_Pretty(long[] Numbers)
    {
        public long Part1(int preambleLength = 25) => (
            from index in Enumerable.Range(preambleLength, Numbers.Length - preambleLength)
            let number = Numbers[index]
            let preamble = Numbers[(index - preambleLength)..index]
            where !IsValid(number, preamble)
            select number
        ).First();

        private bool IsValid(long number, long[] preamble) => (
            from a in Enumerable.Range(0, preamble.Length)
            from b in Enumerable.Range(0, preamble.Length)
            where a != b
            select preamble[a] + preamble[b] == number
        ).Any(x => x);

        public long Part2(int preambleLength = 25)
        {
            var invalidNumber = Part1(preambleLength);

            return (
                from length in Enumerable.Range(2, Numbers.Length - 2)
                from offset in Enumerable.Range(0, Numbers.Length - length)
                let sequence = Numbers[offset..(offset + length)]
                where sequence.Sum() == invalidNumber
                select sequence.Min() + sequence.Max()
            ).First();
        }
    }
}