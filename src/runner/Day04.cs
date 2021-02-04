using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public class Day04
    {
        private readonly Passport[] _passports;

        public Day04(string input)
        {
            var regex = new Regex(@"(?<key>\S*):(?<value>\S*)\s?");

            var passportKeyValueMatches =
                from onePassport in input.Split(Environment.NewLine + Environment.NewLine)
                select regex.Matches(onePassport);
            
            _passports = (
                from passportMatch in passportKeyValueMatches
                select new Passport(
                    from match in passportMatch
                    select (
                        match.GetGroupValue("key"),
                        match.GetGroupValue("value")
                    )
                )
            ).ToArray();
        }

        public int Part1()
            => _passports.Count(pp => pp.ContainsAllProperties(new[] {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}));

        public int Part2()
            => _passports.Count(pp => pp.IsValid());

        record Passport(IReadOnlyDictionary<string, string> Properties)
        {
            public Passport(IEnumerable<(string key, string value)> kv)
                : this(kv.ToDictionary(x => x.key, x => x.value))
            {
            }

            public bool ContainsAllProperties(IEnumerable<string> properties)
                => properties.All(p => Properties.ContainsKey(p));

            public bool IsValid() =>
                GetIntValue("byr") is >= 1920 and <= 2002 &&
                GetIntValue("iyr") is >= 2010 and <= 2020 &&
                GetIntValue("eyr") is >= 2020 and <= 2030 &&
                ValidateHeight(GetValue("hgt")) &&
                ValidateHexColor(GetValue("hcl")) &&
                GetValue("ecl") is "amb" or "blu" or "brn" or "gry" or "grn" or "hzl" or "oth" &&
                ValidatePassportId(GetValue("pid"));

            bool ValidateHeight(string? value) => (height: ToInt(value?[..^2]), unit: value?[^2..]) switch
            {
                {unit: "cm", height: >= 150 and <= 193} => true,
                {unit: "in", height: >= 59 and <= 76} => true,
                _ => false
            };

            bool ValidateHexColor(string? value) =>
                value?[0] == '#' && value.Length == 7 && value[1..].All(c => char.IsDigit(c) || c is >= 'a' and <= 'f');

            bool ValidatePassportId(string? value) => value?.Length == 9 && value.All(char.IsDigit);
            
            string? GetValue(string propertyName) => Properties.TryGetValue(propertyName, out var val) ? val : null;
            int? GetIntValue(string propertyName) => ToInt(GetValue(propertyName));
            int? ToInt(string? value) => int.TryParse(value, out var parsed) ? parsed : null;
        }
    }
}