using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day03 : IDay
    {
        private readonly IList<string> m_input;

        public Day03(string[] input) => m_input = input;

        public string SolveA()
        {
            var dictionary = new Dictionary<(int, int), int>();

            foreach (var line in m_input)
            {
                var temp = line.Split(" #@,:x".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
                    .Select(int.Parse)
                    .ToList();

                var xStart = temp[1];
                var yStart = temp[2];
                var width = temp[3];
                var height = temp[4];

                for (var x = xStart; x < xStart + width; x++)
                {
                    for (var y = yStart; y < yStart + height; y++)
                    {
                        if (dictionary.ContainsKey((x, y)))
                        {
                            dictionary[(x, y)]++;
                        }
                        else
                        {
                            dictionary[(x, y)] = 1;
                        }
                    }
                }
            }

            return dictionary
                .Count(x => x.Value > 1)
                .ToString();
        }

        public string SolveB()
        {
            var dictionary = new Dictionary<(int, int), List<string>>();
            var allIds = new List<string>();

            foreach (var line in m_input)
            {
                var temp = line.Split(" #@,:x".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
                    .Select(int.Parse)
                    .ToList();

                var id = temp[0].ToString();
                var xStart = temp[1];
                var yStart = temp[2];
                var width = temp[3];
                var height = temp[4];

                allIds.Add(id);

                for (var x = xStart; x < xStart + width; x++)
                {
                    for (var y = yStart; y < yStart + height; y++)
                    {
                        if (dictionary.ContainsKey((x, y)))
                        {
                            dictionary[(x, y)].Add(id);
                        }
                        else
                        {
                            dictionary[(x, y)] = new List<string> { id };
                        }
                    }
                }
            }

            var overlappingIds = dictionary
                .Where(x => x.Value.Count > 1)
                .SelectMany(x => x.Value)
                .Distinct()
                .ToList();

            return allIds
                .Except(overlappingIds)
                .Single();
        }
    }
}
