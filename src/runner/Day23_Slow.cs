using System;
using System.Linq;

namespace aoc_runner
{
    public class Day23Slow
    {
        public string Part1(string input) =>
            Enumerable.Range(0, 100)
                      .Aggregate(Circle.Parse(input),
                                 (agg, _) => agg.Next(),
                                 agg => agg.Solution);

        public int Part2Invalid(string input)
        {
            var originalCircle = Circle.Parse(input);
            var circle = new Circle(originalCircle.Cups.Concat(Enumerable.Range(10, 1_000_000 - 9)).ToArray(), originalCircle.CurrentCup);

            // - way to slow to ever finish...
            var millionthState = Enumerable.Range(0,  1)// 10 * 1000 * 1000) 
                                           .Aggregate(circle,
                                                      (agg, _) => agg.Next());

            var elements = millionthState.TakeElements(1, 2).taken.ToArray();
            return elements[0] * elements[1];
        }



        public record Circle(int[] Cups, int CurrentCup)
        {
            public (int[] taken, int[] remaining) TakeElements(int cupId, int count)
            {
                var cupIndex = Array.FindIndex(Cups, x => x == cupId);
                var indicesToTake = Enumerable.Range(1, count).Select(x => (cupIndex + x) % Cups.Length).ToArray();

                return (indicesToTake.Select(i => Cups[i]).ToArray(),
                    Cups.Select((cup, i) => (cup, i)).Where(x => !indicesToTake.Contains(x.i)).Select(x => x.cup).ToArray());
            }

            public Circle Next()
            {
                var (taken, remaining) = TakeElements(CurrentCup, 3);
                var destinationIndex = FindDestinationIndex(remaining);
                var next = remaining[..(destinationIndex + 1)].Concat(taken).Concat(remaining[(destinationIndex + 1)..]).ToArray();
                var nextCurrentCupIndex = (Array.IndexOf(next, CurrentCup) + 1) % Cups.Length;

                return new(next, next[nextCurrentCupIndex]);
            }
            
            int FindDestinationIndex(int[] remaining)
            {
                for (var i = 1; i <= 9; i++)
                {
                    var destination = 1 + (Cups.Length + CurrentCup - 1 - i) % Cups.Length;
                    var destinationIndex = Array.IndexOf(remaining, destination);
                    if (destinationIndex != -1) 
                        return destinationIndex;
                }
                throw new Exception();
            }

            public string Solution => string.Concat(TakeElements(1, Cups.Length-1).taken.Select(x => x.ToString()));
            
            public static Circle Parse(string input) => new(input.Select(x => x - '0').ToArray(), input.First() - '0');

            public override string ToString() => string.Concat(Cups.Select(x => x == CurrentCup ? $" ({x}) " : $"  {x}  "));
        }
    }
}