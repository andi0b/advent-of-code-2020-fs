using System;
using System.Linq;
using System.Threading;

namespace aoc_runner
{
    public class Day11
    {
        private readonly string[] _input;
        private          char[,]  _map;
        private          int      _height;
        private          int      _width;

        public Day11(string[] input) { _input = input; }

        private void ReadInput(string[] input)
        {
            _width  = input[0].Length;
            _height = input.Length;

            _map = new char[_width, _height];

            for (var x = 0; x < _width; x++)
            for (var y = 0; y < _height; y++)
                _map[x, y] = input[y][x];
        }

        public int Part1() => Simulate(OccupiedAdjacentSeats,4);
        public int Part2() => Simulate(OccupiedVisibleSeats,5);

        public int Simulate(Func< int,  int, int,bool> adjacentSeatCount, int triggerSeatCount)
        {
            ReadInput(_input);
            
            bool isDifferent;
            var newMap = new char[_width, _height];

            do
            {
                isDifferent = false;

                for (var y = 0; y < _height; y++)
                for (var x = 0; x < _width; x++)
                {
                    var newValue = newMap[x, y] = _map[x, y] switch
                    {
                        'L' when !adjacentSeatCount(x, y,0) => '#',
                        '#' when adjacentSeatCount(x, y,triggerSeatCount-1) => 'L',
                        var seatType => seatType
                    };

                    if (_map[x, y] != newValue)
                        isDifferent = true;
                }
                
                var intermediate = _map;
                _map   = newMap;
                newMap = intermediate;
            } while (isDifferent);

            return _map.OfType<char>().Count(x => x == '#');
        }

        private static readonly (int x, int y)[] Directions =
        {
            (-1, -1), (0, -1), (1, -1),
            (-1, 0), (1, 0),
            (-1, 1), (0, 1), (1, 1)
        };

        private bool OccupiedAdjacentSeats(int x, int y, int lim)
        {
            var occupiedSeats = 0;
            foreach (var direction in Directions)
            {
                var x1 = direction.x + x;
                var y1 = direction.y + y;
                if (InBounds(x1, y1) && _map[x1, y1] == '#')
                {
                    occupiedSeats++;
                    if (occupiedSeats > lim)
                        return true;
                }
            }

            return false;
        }

        private bool OccupiedVisibleSeats(int x, int y, int lim)
        {
            var occupiedSeats = 0;

            foreach (var direction in Directions)
                for (var distance = 1;; distance++)
                {
                    var x1 = direction.x * distance + x;
                    var y1 = direction.y * distance + y;

                    if (!InBounds(x1, y1)) break;
                    var value = _map[x1, y1];
                    if (value == 'L') break;
                    if (value == '#')
                    {
                        occupiedSeats++;
                        if (occupiedSeats > lim) return true;
                        break;
                    }
                }

            return false;
        }

        public bool InBounds(int x, int y) => x >= 0 && y >= 0 && x < _width && y < _height;
    }
}