
using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public class Day18
    {
        public long Part1(string[] input) =>
            input.Select(x => Expression.Parse(x).Solve())
                 .Sum();

        public long Part2(string[] input) =>
            input.Select(ParenthesisHack)
                 .Select(x => Expression.Parse(x).Solve())
                 .Sum();

        

        public record Expression(object[] Items)
        {
            public long Solve()
            {
                var lhs = (long?) null;
                var op = ' ';
                foreach (var item in Items)
                {
                    long GetValue()=>item switch
                    {
                        (long num) => num,
                        (Expression expression) => expression.Solve(),
                        _ => throw new ArgumentOutOfRangeException()
                    };
                    
                    if (lhs == null)
                    {
                        lhs = GetValue();
                    }
                    else if (item is char c)
                    {
                        op = c;
                    }
                    else
                    {
                        if (op == '+')
                            lhs += GetValue();
                        else if (op == '*')
                            lhs *= GetValue();
                    }
                }

                return lhs.Value;
            }

            public static Expression Parse(string input)
            {
                var items = new List<object>();
                
                var numbers = new List<char>();
                var insideBracket = new List<char>();
                
                var bracketCount = 0;

                foreach (var c in input.Append(' '))
                {
                    if (c == '(')
                        bracketCount++;

                    if (!char.IsDigit(c) && numbers.Any())
                    {
                        items.Add(long.Parse(new string(numbers.ToArray())));
                        numbers.Clear();
                    }
                    else if (c == ')' && bracketCount == 1)
                    {
                        items.Add(Parse(new string(insideBracket.ToArray())));
                        insideBracket.Clear();
                    }
                    else if (bracketCount == 1 && c == '(')
                        continue;
                    else if (bracketCount > 0)
                        insideBracket.Add(c);
                    else if (char.IsDigit(c))
                        numbers.Add(c);
                    else if (c == ' ')
                        continue;
                    else
                        items.Add(c);

                    if (c == ')')
                        bracketCount--;
                }

                return new(items.ToArray());
            }
        }


        public static string ParenthesisHack(string input)
            => "((" +
               input.Replace("(", "((")
                    .Replace(")", "))")
                    .Replace("+", ") + (")
                    .Replace("*", ")) * ((")
               + "))";
    }
}