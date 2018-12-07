using System;
using System.Linq;
using System.Text;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day05 : IDay
    {
        private readonly string m_input;

        public Day05(string[] input) => m_input = input.Single();

        public string SolveA()
        {
            var polymer = new StringBuilder(m_input, m_input.Length);

            for (var i = 0; i + 1 < polymer.Length; i++)
            {
                if (IsMatchingPair(polymer[i], polymer[i + 1]))
                {
                    polymer.Remove(i, 2);
                    i = Math.Max(i - 2, -1);
                }
            }

            return polymer.Length.ToString();
        }

        private bool IsMatchingPair(char x, char y)
            => (Char.IsLower(x) != Char.IsLower(y))
            && (Char.ToLower(x) == Char.ToLower(y));

        public string SolveB()
        {
            return Enumerable.Range('a', 'z' - 'a' + 1)
                .Select(x => GetLengthOfCollapsedPolymer((char)x))
                .Min()
                .ToString();

            int GetLengthOfCollapsedPolymer(char unitToRemove)
            {
                var polymer = new StringBuilder(m_input, m_input.Length);
                polymer.Replace(unitToRemove.ToString(), "");
                polymer.Replace(unitToRemove.ToString().ToUpper(), "");

                for (var i = 0; i + 1 < polymer.Length; i++)
                {
                    if (IsMatchingPair(polymer[i], polymer[i + 1]))
                    {
                        polymer.Remove(i, 2);
                        i = Math.Max(i - 2, -1);
                    }
                }

                return polymer.Length;
            }
        }
    }
}
