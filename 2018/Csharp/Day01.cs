using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day01 : IDay
    {
        private readonly IList<int> m_input;

        public Day01(string[] input)
            => m_input = input
                .Select(int.Parse)
                .ToList();

        public string SolveA()
            => m_input
                .Sum()
                .ToString();

        public string SolveB()
        {
            var current = 0;
            var history = new Dictionary<int, int>();

            while (true)
            {
                foreach (var value in m_input)
                {
                    current += value;

                    if (history.ContainsKey(current))
                    {
                        return current.ToString();
                    }
                    else
                    {
                        history.Add(current, 0);
                    }
                }
            }
        }
    }
}
