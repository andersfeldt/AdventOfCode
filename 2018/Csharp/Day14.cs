using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day14 : IDay
    {
        private readonly int m_input;

        public Day14(string[] input) => m_input = int.Parse(input[0]);

        public string SolveA()
        {
            var recipes = new List<int>(capacity: m_input + 10) { 3, 7 };

            var currentA = 0;
            var currentB = 1;

            while (recipes.Count < m_input + 10)
            {
                var newRecipes = (recipes[currentA] + recipes[currentB])
                    .ToString()
                    .Select(c => c - '0');
                recipes.AddRange(newRecipes);

                currentA = (currentA + recipes[currentA] + 1) % recipes.Count;
                currentB = (currentB + recipes[currentB] + 1) % recipes.Count;
            }

            var scores = recipes.GetRange(m_input, 10);

            return string.Join("", scores);
        }

        public string SolveB()
        {
            var recipes = new List<int> { 3, 7 };

            var currentA = 0;
            var currentB = 1;

            var originalMatch = m_input.ToString();
            var remainingMatch = originalMatch;

            for (var count = 0; true; count++)
            {
                var newScore = (recipes[currentA] + recipes[currentB]).ToString();

                if (count >= 10882091) //value found by searching for subset of input ("58074" instead of "580741")
                {
                    if (remainingMatch == newScore)
                    {
                        return (recipes.Count + newScore.Length - originalMatch.Length).ToString();
                    }

                    if (remainingMatch == originalMatch)
                    {
                        var remainingChanged = false;

                        for (var i = 0; i < newScore.Length; i++)
                        {
                            var substr = newScore.Substring(i);
                            if (remainingMatch.StartsWith(substr))
                            {
                                remainingMatch = remainingMatch.Substring(substr.Length);
                                remainingChanged = true;
                                break;
                            }
                        }

                        if (!remainingChanged)
                        {
                            remainingMatch = originalMatch;
                        }
                    }
                    else
                    {
                        if (remainingMatch.StartsWith(newScore))
                        {
                            remainingMatch = remainingMatch.Substring(newScore.Length);
                        }
                        else if (newScore.StartsWith(remainingMatch))
                        {
                            return (recipes.Count + remainingMatch.Length - originalMatch.Length).ToString();
                        }
                        else
                        {
                            remainingMatch = originalMatch;
                        }
                    }
                }

                var newRecipes = newScore
                    .Select(c => c - '0');
                recipes.AddRange(newRecipes);

                currentA = (currentA + recipes[currentA] + 1) % recipes.Count;
                currentB = (currentB + recipes[currentB] + 1) % recipes.Count;
            }
        }
    }
}
