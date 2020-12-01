﻿using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace aoc_runner.Infrastructure
{
    public class DayRunner
    {
        private static Dictionary<int, Type> DayTypes;

        public static IEnumerable<int> AvailableDays => DayTypes.Keys.OrderBy(x => x);
        
        static DayRunner()
        {
            var assembly = Assembly.GetExecutingAssembly();

            // auto discover solution types
            DayTypes = new Dictionary<int, Type>(
                from type in assembly.GetTypes()
                where type.Namespace=="aoc_runner"
                where type.Name?.StartsWith("Day") == true
                let dayPart = type.Name.Replace("Day", "")
                let parsed = new {canParse = int.TryParse(dayPart, out var parsed), day = parsed}
                where parsed.canParse
                select new KeyValuePair<int, Type>(parsed.day, type)
            );
        }

        private readonly int         _day;
        private readonly InputLoader _loader;
        private readonly object      _dayInstance;
        private readonly Type        _dayType;

        public DayRunner(int day)
        {
            _day    = day;
            _loader = new InputLoader(day);
            
            _dayType = DayTypes[day];
            _dayInstance = Activator.CreateInstance(_dayType) 
                           ?? throw new Exception($"Error Activating type {_dayType.FullName}");
        }
        

        public void Run()
        {
            Console.WriteLine($"-------- Day {_day}:");
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

            var parameterType = method.GetParameters().First().ParameterType;
            
            var parameterValue = parameterType.IsArray
                ? _loader.GetType().GetMethod(nameof(_loader.ReadLines))?
                   .MakeGenericMethod(parameterType.GetElementType()!)
                         .Invoke(_loader, Array.Empty<object>())
                : _loader.GetType().GetMethod(nameof(_loader.Read))?
                   .MakeGenericMethod(parameterType)
                         .Invoke(_loader, Array.Empty<object>());
            
            var sw = Stopwatch.StartNew();
            var returnValue = method.Invoke(_dayInstance, new[] {parameterValue});
            sw.Stop();

            var elapsedPadded = $"{sw.Elapsed.TotalMilliseconds:0.00}".PadLeft(7);
            Console.WriteLine($"finished after {elapsedPadded}ms, with result: {returnValue}");
        }
    }
}