using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace aoc_runner.Infrastructure
{
    public class DayRunner
    {
        public static ILookup<int, Type> DayTypes;

        public static IEnumerable<int> AvailableDays => DayTypes.Select(x=>x.Key).OrderBy(x => x);
        
        static DayRunner()
        {
            var assemblies = new[]
            {
                Assembly.GetExecutingAssembly(), typeof(solutions.Say).Assembly
            };

            // auto discover solution types
            DayTypes =
            (
                from assembly in assemblies
                from type in assembly.GetTypes()
                where type.Namespace == "aoc_runner" || type.Namespace == "solution"
                where type.Name?.StartsWith("Day") == true
                let dayPart = type.Name[3..5]
                let parsed = new {canParse = int.TryParse(dayPart, out var parsed), day = parsed}
                where parsed.canParse
                select new KeyValuePair<int, Type>(parsed.day, type)
            ).ToLookup(x => x.Key, x => x.Value);
        }

        private readonly int         _day;
        private readonly InputLoader _loader;
        private readonly object      _dayInstance;
        private readonly Type        _dayType;
        private readonly TimeSpan    _initTime;

        public static void Run(int day)
        {
            if (!DayTypes.Contains(day)) throw new ArgumentException($"Day {day} not found", nameof(day));
            foreach (var type in DayTypes[day])
                new DayRunner(day, type).RunInternal();
        }

        public static void Run()
        {
            foreach (var day in AvailableDays)
                Run(day);
        }


        private DayRunner(int day, Type dayType)
        {
            _day    = day;
            _loader = new InputLoader(day);

            _dayType = dayType;

            var sw = Stopwatch.StartNew();
            _dayInstance = CreateDayInstance() 
                           ?? throw new Exception($"Error Activating type {_dayType.FullName}");
            sw.Stop();
            _initTime = sw.Elapsed;
        }

        private object? CreateDayInstance()
        {
            var firstConstructor = _dayType.GetConstructors().Last();
            var parameterType = firstConstructor.GetParameters().FirstOrDefault()?.ParameterType;

            if (parameterType == null)
                return Activator.CreateInstance(_dayType);
            
            var parameters = new[] {GetParameterValue(parameterType!)};
            return Activator.CreateInstance(_dayType, parameters);
        }

        private void RunInternal()
        {
            Console.WriteLine($"-------- Day {_day} ({_dayType.Name}) (init took {_initTime.TotalMilliseconds:0.00}ms):");
            ExecutePart(1);
            ExecutePart(2);
            
            Console.WriteLine();
            Console.WriteLine();
        }

        private void ExecutePart(int partId)
        {
            var methodName = $"Part{partId}";
            var method = _dayType.GetMethod(methodName);

            if (method == null)
            {
                Console.Write($"Method for Part {partId} not found");
                return;
            }
            
            Console.Write($"Part {partId}... ");

            var firstParameter = method.GetParameters().FirstOrDefault();
            var parameterType = firstParameter?.ParameterType;

            var parameterCount = method.GetParameters().Length;

            var parameters = firstParameter == null
                ? new object[0]
                : firstParameter.IsOptional
                    ? Enumerable.Repeat(Type.Missing, parameterCount).ToArray()
                    : new[] {GetParameterValue(parameterType!)};

            var sw = Stopwatch.StartNew();
            var returnValue = method.Invoke(_dayInstance, parameters);
            sw.Stop();

            var elapsedPadded = $"{sw.Elapsed.TotalMilliseconds:0.00}".PadLeft(7);
            Console.WriteLine($"finished after {elapsedPadded}ms, with result: {returnValue}");
        }

        private object? GetParameterValue(Type parameterType) =>
            parameterType.IsArray
                ? _loader.GetType()
                         .GetMethod(nameof(_loader.ReadLines))
                        ?.MakeGenericMethod(parameterType.GetElementType()!)
                         .Invoke(_loader, Array.Empty<object>())
                : _loader.GetType()
                         .GetMethod(nameof(_loader.Read))
                        ?.MakeGenericMethod(parameterType)
                         .Invoke(_loader, Array.Empty<object>());
    }
}