using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day12 : IDay
    {
        private readonly IList<string> m_input;

        public Day12(string[] input) => m_input = input;

        public string SolveA()
        {
            const int numberOfGenerations = 20;

            var rules = CreateRules();
            var pots = CreatePots();

            for (var i = 0; i < numberOfGenerations; i++)
            {
                pots = GrowNewGeneration(rules, pots);
                Display(pots);
            }

            return pots
                .Where(kvp => kvp.Value == '#')
                .Sum(kvp => kvp.Key)
                .ToString();
        }

        private void Display(IDictionary<int, char> pots)
        {
            var orderedPots = pots
                .OrderBy(x => x.Key)
                .ToList();

            Console.Write($"{orderedPots[0].Key} ");

            foreach (var kvp in orderedPots)
            {
                Console.Write(kvp.Value);
            }

            Console.WriteLine();
        }

        private IDictionary<string, char> CreateRules()
        {
            return m_input
                .Where(s => s.Contains("=>"))
                .Select(Parse)
                .ToDictionary(x => x.Key, x => x.Value);

            (string Key, char Value) Parse(string s)
            {
                var parts = s.Split(" =>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                return (parts[0], parts[1][0]);
            }
        }

        private IDictionary<int, char> CreatePots()
        {
            return m_input[0]
                .Where(c => c == '.' || c == '#')
                .Select((c, i) => (Key: i, Value: c))
                .ToDictionary(x => x.Key, x => x.Value);
        }

        private IDictionary<int, char> GrowNewGeneration(IDictionary<string, char> rules, IDictionary<int, char> pots)
        {
            const int emptyPotsAtEdgesLimit = 1;

            if (ShouldAddEmptyPots(pots.OrderBy(kvp => kvp.Key)))
            {
                var min = pots.Keys.Min();
                pots[min - 1] = '.';
                pots[min - 2] = '.';
            }

            if (ShouldAddEmptyPots(pots.OrderByDescending(kvp => kvp.Key)))
            {
                var max = pots.Keys.Max();
                pots[max + 1] = '.';
                pots[max + 2] = '.';
            }

            return pots.Keys
                .ToDictionary(i => i, i => GetNextState(rules, pots, i));

            bool ShouldAddEmptyPots(IEnumerable<KeyValuePair<int, char>> orderedPots)
                => orderedPots
                    .TakeWhile(kvp => kvp.Value == '.')
                    .Count() < emptyPotsAtEdgesLimit;
        }

        private char GetNextState(IDictionary<string, char> rules, IDictionary<int, char> pots, int index)
        {
            var surroundings = Enumerable.Range(-2, 5)
                .Select(i => index + i)
                .Select(GetValue)
                .ToArray();

            return rules[new string(surroundings)];

            char GetValue(int i)
                => pots.TryGetValue(i, out var value)
                    ? value
                    : '.';
        }

        public string SolveB()
        {
            throw new System.NotImplementedException();
        }
    }
}
