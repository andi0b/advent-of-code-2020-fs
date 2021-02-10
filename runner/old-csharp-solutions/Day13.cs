using System.Linq;
using System.Numerics;

namespace aoc_runner
{
    public record Day13(int Earliest, int[] BusNumbers)
    {
        public Day13(params string[] input):this(
            int.Parse(input[0]),
            input[1].Split(',').Select(x=>int.TryParse(x, out var parsed) ? parsed : 0).ToArray()) { }

        public int Part1()
        {

            var departures =
                from bn in BusNumbers
                where bn > 0
                select (departureTime: NextDepartureTime(Earliest, bn), bn);

            var (departureTime, busNumber) = departures.Min();
            return (departureTime - Earliest) * busNumber;
        }
        
        private int NextDepartureTime(int afterTimestamp, int busNumber) => (afterTimestamp / busNumber + 1) * busNumber;
        
        
        
        public object Part2(string input) =>
            ChineseRemainderTheorem(
                Parse(input).buses
                            .Select(bus => (mod: bus.period, a: bus.period - bus.delay))
                            .ToArray()
            );

        (int earliestDepart, (long period, int delay)[] buses) Parse(string input) {
            var lines = input.Split("\n");
            var earliestDepart = int.Parse(lines[0]);
            var buses = lines[1].Split(",")
                                .Select((part, idx) => (part, idx))
                                .Where(item => item.part != "x")
                                .Select(item => (period: long.Parse(item.part), delay: item.idx))
                                .ToArray();
            return (earliestDepart, buses);
        }

        // https://rosettacode.org/wiki/Chinese_remainder_theorem#C.23
        long ChineseRemainderTheorem((long mod, long a)[] items) {
            var prod = items.Aggregate(1L, (acc, item) => acc * item.mod);
            var sum = items.Select((item, i) => {
                var p = prod / item.mod;
                return item.a * ModInv(p, item.mod) * p;
            }).Sum();

            return sum % prod;
        }

        long ModInv(long a, long m) => (long)BigInteger.ModPow(a, m - 2, m);
    }
}