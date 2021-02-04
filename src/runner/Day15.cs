using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day15(int[] StartingNumbers)
    {
        public Day15() : this(new[] {11,18,0,20,1,7,16}) { }

        public int Part1() => SolveFast(2020);
        public int Part2() => SolveFast(30000000);
        
        public int Solve(int num)
        {
            var spokenNumbers = new List<int>(num);
            spokenNumbers.AddRange(StartingNumbers);

            int EndOffset(int num) => (
                from x in spokenNumbers!.AsEnumerable().Reverse().Skip(1).Select((spokenNumber, n) => (spokenNumber, n))
                where x.spokenNumber == num
                select x.n + 1
            ).FirstOrDefault();

            while (spokenNumbers.Count < num)
            {
                spokenNumbers.Add(EndOffset(spokenNumbers.Last()));
            }

            return spokenNumbers.Last();
        }

        public int SolveFast(int num)
        {
            var spokenNumbers = StartingNumbers.Take(StartingNumbers.Length - 1)
                                               .Select((spokenNumber, n) => (spokenNumber, n))
                                               .ToDictionary(x => x.spokenNumber, x => x.n);

            var lastSpokenNumber = StartingNumbers.Last();
            for (var i = StartingNumbers.Length - 1; i <= num-2; i++)
            {
                var nextNumber =
                    spokenNumbers.TryGetValue(lastSpokenNumber, out var previouslySpokenRound)
                        ? i  - previouslySpokenRound
                        : 0;
                
                spokenNumbers[lastSpokenNumber] = i;

                lastSpokenNumber = nextNumber;
            }

            return lastSpokenNumber;
        }
    }
}