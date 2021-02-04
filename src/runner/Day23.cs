using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day23(string Input)
    {
        public string Part1() => string.Concat(Solve(9, 100).RightOf(1, 8));
        public long Part2() => Solve(1_000_000, 10_000_000).RightOf(1, 2).Aggregate(1L, (acc, next) => acc * next);

        private CupCircle Solve(int cupCount, int moveCount)
        {
            var circle = new CupCircle(Input, cupCount);
            circle.Move(moveCount);
            return circle;
        }

        public class CupCircle
        {
            private readonly int[] _cups;

            private int _currentCup;

            public CupCircle(string input, int cupCount)
            {
                _cups = Enumerable.Range(2, cupCount).ToArray();
                var inputNumbers = input.Select(x => int.Parse(x.ToString())).ToArray();
                for (var cupIndex = 0; cupIndex < inputNumbers.Length-1; cupIndex++)
                {
                    var inputCupId = inputNumbers[cupIndex];
                    var cupToRightId = inputNumbers[cupIndex + 1];
                    this[inputCupId] = cupToRightId;
                }

                if (inputNumbers.Length < cupCount)
                {
                    this[inputNumbers.Last()] = inputNumbers.Length + 1;
                    this[_cups.Length]        = inputNumbers.First();
                }
                else
                    this[inputNumbers.Last()] = inputNumbers.First();

                _currentCup = inputNumbers.First();
            }

            public void Move(int times)
            {
                var next4 = new int[4];
                for (var i = 0; i < times; i++)
                {
                    RightOf(_currentCup, 4, next4);

                    // take out 3 cups (they are still linked together)
                    this[_currentCup] = next4[3];
                    
                    // find cup to insert after
                    var insertAfter = _currentCup;
                    do insertAfter = 1 + (_cups.Length - 1 + insertAfter - 1) % _cups.Length;
                    while (insertAfter == next4[0] || insertAfter == next4[1] || insertAfter == next4[2]);

                    // insert the 3 cups
                    this[next4[2]]    = this[insertAfter];
                    this[insertAfter] = next4[0];
                    
                    // set new current cup to right of current cup 
                    _currentCup = this[_currentCup];
                }
            }
            
            public int this[int cupId]
            {
                get => _cups[MapIndex(cupId)];
                set => _cups[MapIndex(cupId)] = value;
            }

            private int MapIndex(int cupId) => (_cups.Length + cupId - 1) % _cups.Length;

            public int[] RightOf(int cupId, int count)
            {
                var returnArray = new int[count];
                RightOf(cupId, count, returnArray);
                return returnArray;
            }
            
            public void RightOf(int cupId, int count, in int[] buffer)
            {
                buffer[0] = this[cupId];
                for (var i = 1; i < count; i++)
                {
                    buffer[i] = this[buffer[i - 1]];
                }
            }
        }
    }
}