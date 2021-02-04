using System.Linq;

namespace aoc_runner
{
    public class Day01
    {
        public int Part1(int[] expenseReports)
            => (
                from x in expenseReports
                from y in expenseReports
                where x + y == 2020
                select x * y
            ).First();

        public int Part2(int[] expenseReports)
            => (
                from x in expenseReports
                from y in expenseReports
                from z in expenseReports
                where x + y + z == 2020
                select x * y * z
            ).First();
    }
}