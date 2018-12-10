using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day09 : IDay
    {
        private readonly string m_input;

        public Day09(string[] input) => m_input = input.Single();

        public string SolveA()
        {
            var (numberOfPlayers, valueOfLastMarble) = Parse(m_input);
            return GetMaxScore(numberOfPlayers, valueOfLastMarble).ToString();
        }

        private (int, int) Parse(string str)
        {
            var words = str.Split(' ');
            return (int.Parse(words[0]), int.Parse(words[6]));
        }

        private long GetMaxScore(int numberOfPlayers, int valueOfLastMarble)
        {
            var scoresByPlayer = Enumerable.Range(0, numberOfPlayers).ToDictionary(i => i, _ => 0L);

            var ring = new List<int>(valueOfLastMarble) { 0 };
            var currentIndex = 0;
            var currentPlayer = 0;

            for (var nextMarble = 1; nextMarble <= valueOfLastMarble; nextMarble++)
            {
                if (nextMarble % 23 == 0)
                {
                    var removeIndex = GetCircularIndex(ring.Count, currentIndex, steps: -7);

                    scoresByPlayer[currentPlayer] += nextMarble + ring[removeIndex];

                    currentIndex = removeIndex == ring.Count - 1
                        ? 0
                        : removeIndex;

                    ring.RemoveAt(removeIndex);
                }
                else
                {
                    currentIndex = GetCircularIndex(ring.Count, currentIndex, steps: 2);
                    ring.Insert(currentIndex, nextMarble);
                }

                currentPlayer = GetNextPlayer(numberOfPlayers, currentPlayer);
            }

            return scoresByPlayer.Values.Max();
        }

        private int GetNextPlayer(int numberOfPlayers, int currentPlayer)
        {
            return (currentPlayer + 1) % numberOfPlayers;
        }

        private int GetCircularIndex(int totalCount, int currentIndex, int steps)
        {
            var newIndex = (currentIndex + steps) % totalCount;

            return newIndex >= 0
                ? newIndex
                : newIndex + totalCount;
        }

        public string SolveB()
        {
            var (numberOfPlayers, valueOfLastMarble) = Parse(m_input);

            //Very slow solution... :(

            return GetMaxScore(numberOfPlayers, 100 * valueOfLastMarble).ToString();
        }
    }
}
