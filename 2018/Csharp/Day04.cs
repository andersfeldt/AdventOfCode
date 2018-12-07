using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day04 : IDay
    {
        private readonly IList<string> m_input;
        private static Regex ParseRegex = new Regex(@"\[([^\]]+)\]( Guard #(\d+))? (.*)", RegexOptions.IgnoreCase);

        public Day04(string[] input) => m_input = input;

        public string SolveA()
        {
            var selected = GetGroupedDatas()
                .Select(ExtractSleepingInfo)
                .OrderByDescending(x => x.TotalMinuteCount)
                .First();

            return (selected.GuardId * selected.MostFrequentMinute).ToString();
        }

        private ILookup<int, Data> GetGroupedDatas()
        {
            var datas = m_input
                .Select(Parse)
                .OrderBy(x => x.Date)
                .ToList();

            var currentGuard = 0;

            foreach (var data in datas)
            {
                if (data.GuardId != null)
                {
                    currentGuard = data.GuardId.Value;
                }
                else
                {
                    data.GuardId = currentGuard;
                }
            }

            return datas.ToLookup(x => x.GuardId.Value);

            Data Parse(string str)
            {
                var match = ParseRegex.Match(str);

                return new Data
                {
                    Date = DateTime.Parse(match.Groups[1].Value),
                    GuardId = match.Groups[3].Success
                        ? int.Parse(match.Groups[3].Value)
                        : (int?)null,
                    Asleep = match.Groups[4].Value == "falls asleep"
                };
            }
        }

        private SleepingInfo ExtractSleepingInfo(IGrouping<int, Data> grouping)
        {
            var allMinutes = new List<int>();
            var datas = grouping.ToList();

            for (var i = 1; i < datas.Count; i++)
            {
                var first = datas[i - 1];
                var second = datas[i];

                if (first.Asleep)
                {
                    allMinutes.AddRange(GetInterestingMinutes(first.Date, second.Date));
                }
            }

            return new SleepingInfo
            {
                GuardId = grouping.Key,
                TotalMinuteCount = allMinutes.Count,
                MostFrequentMinute = GetMostFrequent(allMinutes)
            };

            IEnumerable<int> GetInterestingMinutes(DateTime start, DateTime end)
            {
                var startMinute = start.Hour == 0
                    ? start.Minute
                    : 0;
                var endMinute = end.Hour == 0
                    ? end.Minute
                    : 59;

                return Enumerable.Range(startMinute, endMinute - startMinute);
            }

            int GetMostFrequent(IList<int> values)
            {
                if (!values.Any())
                {
                    return -1;
                }

                return values
                    .GroupBy(x => x)
                    .Select(x => (Minute: x.Key, Count: x.Count()))
                    .OrderByDescending(x => x.Count)
                    .Select(x => x.Minute)
                    .First();
            }
        }

        private class Data
        {
            public DateTime Date { get; set; }
            public int? GuardId { get; set; }
            public bool Asleep { get; set; }
        }

        private class SleepingInfo
        {
            public int GuardId { get; set; }
            public int TotalMinuteCount { get; set; }
            public int MostFrequentMinute { get; set; }
        }

        public string SolveB()
        {
            var selected = GetGroupedDatas()
                .Select(ExtractSleepingInfoB)
                .OrderByDescending(x => x.OccurencesOfMostFrequentMinute)
                .First();

            return (selected.GuardId * selected.MostFrequentMinute).ToString();
        }

        private SleepingInfoB ExtractSleepingInfoB(IGrouping<int, Data> grouping)
        {
            var allMinutes = new List<int>();
            var datas = grouping.ToList();

            for (var i = 1; i < datas.Count; i++)
            {
                var first = datas[i - 1];
                var second = datas[i];

                if (first.Asleep)
                {
                    allMinutes.AddRange(GetInterestingMinutes(first.Date, second.Date));
                }
            }

            var mostFrequentMinute = GetMostFrequent(allMinutes);

            return new SleepingInfoB
            {
                GuardId = grouping.Key,
                TotalMinuteCount = allMinutes.Count,
                MostFrequentMinute = mostFrequentMinute,
                OccurencesOfMostFrequentMinute = allMinutes.Count(x => x == mostFrequentMinute)
            };

            IEnumerable<int> GetInterestingMinutes(DateTime start, DateTime end)
            {
                var startMinute = start.Hour == 0
                    ? start.Minute
                    : 0;
                var endMinute = end.Hour == 0
                    ? end.Minute
                    : 59;

                return Enumerable.Range(startMinute, endMinute - startMinute);
            }

            int GetMostFrequent(IList<int> values)
            {
                if (!values.Any())
                {
                    return -1;
                }

                return values
                    .GroupBy(x => x)
                    .Select(x => (Minute: x.Key, Count: x.Count()))
                    .OrderByDescending(x => x.Count)
                    .Select(x => x.Minute)
                    .First();
            }
        }

        private class SleepingInfoB : SleepingInfo
        {
            public int OccurencesOfMostFrequentMinute { get; set; }
        }
    }
}
