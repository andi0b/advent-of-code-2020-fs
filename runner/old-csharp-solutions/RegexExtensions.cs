﻿using System;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public static class RegexExtensions
    {
        public static string GetGroupValue(this Match match, int captureId)
            => match.Groups[captureId].Value;

        public static T GetGroupValue<T>(this Match match, int captureId)
            => (T) Convert.ChangeType(GetGroupValue(match, captureId), typeof(T));

        public static string GetGroupValue(this Match match, string captureName)
            => match.Groups[captureName].Value;
        
        public static T GetGroupValue<T>(this Match match, string captureName)
            => (T) Convert.ChangeType(GetGroupValue(match,captureName), typeof(T));
    }
}