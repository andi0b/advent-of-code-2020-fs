using System;
using System.Linq;
using System.Security.Cryptography;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public class Day02
    {
        public int Part1(string[] inputs)
            => inputs.Select(ParseInput)
                     .Count(x => IsPasswordValid(x.password, x.policy));

        public int Part2(string[] inputs)
            => inputs.Select(ParseInput)
                     .Count(x => IsPasswordValid2(x.password, x.policy));
        
        public bool IsPasswordValid(string password, PasswordPolicy policy)
        {
            var policyCharCount = password.ToCharArray().Count(c => c == policy.Char);
            return policyCharCount >= policy.Min && policyCharCount <= policy.Max;
        }

        public bool IsPasswordValid2(string password, PasswordPolicy policy)
            => password[policy.Min-1] == policy.Char ^ password[policy.Max-1] == policy.Char;

        public (string password, PasswordPolicy policy) ParseInput(string input)
        {
            var match = new Regex(@"(\d*)-(\d*) (\w): (.*)").Match(input);
            
            return (match.GetGroupValue<string>(4),
                new PasswordPolicy(match.GetGroupValue<char>(3),
                                   match.GetGroupValue<int>(1),
                                   match.GetGroupValue<int>(2)));
        }
    }

    public record PasswordPolicy (char Char, int Min, int Max);
}