using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day17(Day17.PocketDimension InitialDimension)
    {
        public Day17(string input) :this(PocketDimension.Parse(input)) { }

        public int Part1() => Solve(InitialDimension);
        public int Part2() => Solve(InitialDimension with {Enable4ThDimension = true});

        public int Solve(PocketDimension dimension) =>
            Enumerable.Repeat(true, 6)
                      .Aggregate(dimension, (acc, _) => acc.NextIteration())
                      .CubesOn.Count;
        
        public record PocketDimension(HashSet<long> CubesOn, bool Enable4ThDimension=false)
        {
            public PocketDimension NextIteration()
            {
                var neighbourDirections = GetNeighbourDirections();
                
                var (boundsX, boundsY, boundsZ, boundsW) = GetBounds();

                var next = new HashSet<long>();
                
                if (!Enable4ThDimension) boundsW = (1, -1);
                for (var w = boundsW.min - 1; w <= boundsW.max + 1; w++)
                for (var z = boundsZ.min - 1; z <= boundsZ.max + 1; z++)
                for (var y = boundsY.min - 1; y <= boundsY.max + 1; y++)
                for (var x = boundsX.min - 1; x <= boundsX.max + 1; x++)
                {
                    var onNeighbour =
                        from direction in neighbourDirections
                        where CubesOn.Contains(Encode(x + direction.x, y + direction.y, z + direction.z, w + direction.w))
                        select direction;
                    
                    // just count until we reach 4
                    var onNeighbourCount = onNeighbour.Take(4).Count();

                    var cur = Encode(x, y, z, w);
                    var curValue = CubesOn.Contains(cur);

                    var newValue = (onNeighbourCount, curValue) switch
                    {
                        (2 or 3, true) => true,
                        (3, false) => true,
                        _ => false
                    };

                    if (newValue)
                        next.Add(cur);
                }

                return this with { CubesOn = next};
            }

            public static PocketDimension Parse(string input)
                => new(new HashSet<long>(
                           from line in input.Split(Environment.NewLine).Select((value, num) => (value, num))
                           from chr in line.value.Select((value, num) => (value, num))
                           where chr.value == '#'
                           select Encode(chr.num, line.num, 0, 0)
                       ));

            private ((int min, int max) x, (int min, int max) y, (int min, int max) z, (int min, int max) w) GetBounds() =>
                CubesOn.Aggregate((
                                      x: (min: int.MaxValue, max: int.MinValue),
                                      y: (min: int.MaxValue, max: int.MinValue),
                                      z: (min: int.MaxValue, max: int.MinValue),
                                      w: (min: int.MaxValue, max: int.MinValue)
                                  ),
                                  (acc, next) =>
                                  {
                                      var nxd = Decode(next);
                                      return (
                                          (Math.Min(acc.x.min, nxd.x), Math.Max(acc.x.max, nxd.x)),
                                          (Math.Min(acc.y.min, nxd.y), Math.Max(acc.y.max, nxd.y)),
                                          (Math.Min(acc.z.min, nxd.z), Math.Max(acc.z.max, nxd.z)),
                                          (Math.Min(acc.w.min, nxd.w), Math.Max(acc.w.max, nxd.w))
                                      );
                                  });

            public static long Encode(in int x, in int y, in int z, in int w) => ((long) x << 48) + ((long) y << 32) + ((long) z << 16) + w;

            public static (int x, int y, int z, int w) Decode(long l) => ((int) (l >> 48), (short) (l >> 32), (short) (l >> 16), (short) l);
            
            private (int x, int y, int z, int w)[] GetNeighbourDirections() => (
                from x in Enumerable.Range(-1, 3)
                from y in Enumerable.Range(-1, 3)
                from z in Enumerable.Range(-1, 3)
                from w in Enable4ThDimension ? Enumerable.Range(-1, 3) : new[] {0}
                where !(x == 0 && y == 0 && z == 0 && w == 0)
                select (x, y, z, w)
            ).ToArray();
        }
    }
}