using System;
using System.Diagnostics;
using System.Linq;

namespace aoc_runner
{
    public record Day12((char, int)[] Instructions)
    {
        public Day12(string[] input) : this(input.Select(i => (i[0], int.Parse(i[1..]))).ToArray()) { }

        public int Part1() => Solve(new ShipPosition());
        public int Part2() => Solve(new ShipWaypointPosition(new(10, 1)));

        private int Solve<T>(T seed) where T : Base<T> =>
            Instructions.Aggregate(
                seed,
                (acc, nextInstruction) => acc.NextPosition(nextInstruction),
                acc => acc.ManhattanDistanceTo());

        record Base<T>(int X = 0, int Y = 0) where T : Base<T>
        {
            private T Derived => (this as T)!;

            public virtual T NextPosition((char, int) instruction) => Derived;

            public T Travel((char direction, int distance) instruction) => instruction.direction switch
            {
                'N' => Derived with{Y = Y + instruction.distance},
                'S' => Derived with{Y = Y - instruction.distance},
                'W' => Derived with{X = X - instruction.distance},
                'E' => Derived with{X = X + instruction.distance},
            };

            public int ManhattanDistanceTo(int x = 0, int y = 0) => Math.Abs(X - x) + Math.Abs(Y - y);
        }

        record ShipPosition(int X = 0, int Y = 0, int HeadingDegrees = 90) : Base<ShipPosition>(X, Y)
        {
            public override ShipPosition NextPosition((char, int) instruction) => instruction switch
            {
                ('R', { } degrees) => this with {HeadingDegrees = (HeadingDegrees + degrees) % 360},
                ('L', { } degrees) => this with {HeadingDegrees = (360 + HeadingDegrees - degrees) % 360},
                ('F', { } distance) => Travel((HeadingDirection, distance)),
                _ => Travel(instruction)
            };

            private char HeadingDirection => HeadingDegrees switch
            {
                0 => 'N',
                90 => 'E',
                180 => 'S',
                270 => 'W'
            };
        }

        record Waypoint(int X, int Y) : Base<Waypoint>(X, Y)
        {
            public Waypoint TurnRight() => new(Y, -X);
            public Waypoint TurnLeft() => new(-Y, X);
            public Waypoint TurnHalfWay() => new Waypoint(-X, -Y);
        }

        record ShipWaypointPosition(Waypoint Waypoint, int X = 0, int Y = 0) : Base<ShipWaypointPosition>(X, Y)
        {
            public override ShipWaypointPosition NextPosition((char, int) instruction) => instruction switch
            {
                ('F', { } times) => this with{ X = X + Waypoint.X * times, Y = Y + Waypoint.Y * times },
                ('R', 90) or ('L', 270) => this with{ Waypoint = Waypoint.TurnRight() },
                ('L', 90) or ('R', 270) => this with{ Waypoint = Waypoint.TurnLeft() },
                ('L' or 'R', 180) => this with{ Waypoint = Waypoint.TurnHalfWay() },
                _ => this with { Waypoint = Waypoint.Travel(instruction) }
            };
        }
    }
}