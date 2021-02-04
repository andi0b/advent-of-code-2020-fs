using System;
using System.Linq;
using aoc_runner.Infrastructure;

if (int.TryParse(args.FirstOrDefault(), out var dayParam))
{
    if (DayRunner.AvailableDays.Contains(dayParam))
        DayRunner.Run(dayParam);
    else
        Console.WriteLine($"Can't find Day {dayParam}, exiting");
}
else
    DayRunner.Run();