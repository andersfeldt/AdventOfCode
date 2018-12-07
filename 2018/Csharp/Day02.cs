using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day02 : IDay
    {
        private readonly IList<string> m_input;

        public Day02(string[] input) => m_input = input;

        public string SolveA()
        {
            var values = m_input
                .Select(GetMultiples)
                .ToList();

            var twos = values.Count(x => x.Contains(2));
            var threes = values.Count(x => x.Contains(3));

            return (twos * threes).ToString();

            IList<int> GetMultiples(string str)
                => str.ToCharArray()
                    .GroupBy(c => c)
                    .Select(x => x.Count())
                    .Distinct()
                    .ToList();
        }

        public string SolveB()
        {
            for (var i = 0; i < m_input.Count; i++)
            {
                var commonLetters = m_input
                    .Select(x => x.Remove(i, 1))
                    .GroupBy(x => x)
                    .Where(x => x.Count() > 1)
                    .FirstOrDefault()
                    ?.Key;

                if (commonLetters != null)
                {
                    return commonLetters;
                }
            }

            return null;
        }
    }
}
