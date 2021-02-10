using System;
using System.IO;
using System.Linq;

namespace aoc_runner.Infrastructure
{
    public class InputLoader
    {
        private readonly int _day;
        
        public InputLoader(int day)
        {
            _day = day;
        }
        
        private string GetPath() => Path.Combine("inputs", $"input_day_{_day:00}.txt");
        
        public string ReadAllText() => File.ReadAllText(GetPath());
        public string[] ReadAllLines() => File.ReadAllLines(GetPath());

        public T Read<T>() => (T) Convert.ChangeType(ReadAllText(), typeof(T));
        public T[] ReadLines<T>() => ReadAllLines().Select(x => (T) Convert.ChangeType(x, typeof(T))).ToArray();
    }
}