
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public record Day16(Day16.Rule[] Rules, int[] MyTicket, int[][] NearbyTickets)
    {
        public Day16(string input) : this(Parse(input)) { }


        public int Part1() => (
            from ticket in NearbyTickets
            from number in ticket
            where !Rules.Any(r => r.InRanges(number))
            select number
        ).Sum();

        public long Part2(string startsWith="departure")
        {
            var validTickets =
                NearbyTickets
                   .Prepend(MyTicket)
                   .Where(t => t.All(num => Rules.Any(r => r.InRanges(num))))
                   .ToArray();

            var availableRules = Rules.ToList();

            var ruleMapping = new Dictionary<int, Rule>();

            while (availableRules.Any())
                for (var column = 0; column < MyTicket.Length; column++)
                {
                    var matchingRules = availableRules.Where(r => validTickets.Select(x => x[column]).All(r.InRanges))
                                                      .Take(2).ToArray();

                    if (matchingRules.Length != 1)
                        continue;

                    var matchingRule = matchingRules[0];

                    availableRules.Remove(matchingRule);
                    ruleMapping.Add(column, matchingRule);
                }


            var selectedRules = ruleMapping.Where(kv => kv.Value.Name.StartsWith(startsWith));
            var selectedColumnIds = selectedRules.Select(kv => kv.Key).ToArray();

            return MyTicket.Select((number, n) => (number, n))
                           .Where(x => selectedColumnIds.Contains(x.n))
                           .Select(x => x.number)
                           .Aggregate(1L, (acc, next) => acc * next);
        }
        
        private static Day16 Parse(string input)
        {
            var ruleMatches = Regex.Matches(input, @"(.*): (\d*)-(\d*) or (\d*)-(\d*)");
            var myTicketMatch = Regex.Match(input, @"your ticket:(?:\r\n|\n|\r)(.*)(?:\r)?", RegexOptions.Multiline);
            var nearbyTicketsMatch = Regex.Match(input, @"nearby tickets:(?:\r\n|\n|\r)(.*)", RegexOptions.Multiline | RegexOptions.Singleline);

            var rules = (
                from m in ruleMatches
                let range1 = new Range(int.Parse(m.Groups[2].Value), int.Parse(m.Groups[3].Value))
                let range2 = new Range(int.Parse(m.Groups[4].Value), int.Parse(m.Groups[5].Value))
                select new Rule(m.Groups[1].Value, range1, range2)
            ).ToArray();

            int[] ParseTicket(string ti) => ti.Split(',').Select(int.Parse).ToArray();
            
            var myTicket = ParseTicket(myTicketMatch.Groups[1].Value);

            var nearbyTickets = nearbyTicketsMatch.Groups[1].Value.Split(Environment.NewLine).Select(ParseTicket).ToArray();

            return new (rules, myTicket, nearbyTickets);
        }

        public record Rule(string Name, Range Range1, Range Range2)
        {
            public Range[] Ranges => new[] {Range1, Range2};
            public bool InRanges(int num) => Range1.InRange(num) || Range2.InRange(num);
        }

        public record Range(int From, int To)
        {
            public bool InRange(int num) => num >= From && num <= To;
        }
    }
}