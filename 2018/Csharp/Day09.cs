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
            valueOfLastMarble *= 100;

            var scoresByPlayer = Enumerable.Range(0, numberOfPlayers).ToDictionary(i => i, _ => 0L);
            var currentPlayer = 1;

            var ring = new Ring(0);

            for (var nextMarble = 1; nextMarble <= valueOfLastMarble; nextMarble++)
            {
                if (nextMarble % 23 == 0)
                {
                    ring.Step(-7);
                    scoresByPlayer[currentPlayer] += nextMarble + ring.CurrentValue;
                    ring.Remove();
                }
                else
                {
                    ring.Step(1);
                    ring.Insert(nextMarble);
                }

                currentPlayer = GetNextPlayer(numberOfPlayers, currentPlayer);
            }

            return scoresByPlayer.Values.Max().ToString();
        }

        private class Ring
        {
            public Ring(int value)
            {
                var item = new Item(value);

                item.Previous = item;
                item.Next = item;

                CurrentItem = item;
            }

            private Item CurrentItem { get; set; }

            public int CurrentValue => CurrentItem.Value;

            public void Step(int steps)
            {
                while (steps > 0)
                {
                    CurrentItem = CurrentItem.Next;
                    steps--;
                }

                while (steps < 0)
                {
                    CurrentItem = CurrentItem.Previous;
                    steps++;
                }
            }

            public void Insert(int value)
            {
                var newItem = new Item(value)
                {
                    Previous = CurrentItem,
                    Next = CurrentItem.Next
                };

                newItem.Previous.Next = newItem;
                newItem.Next.Previous = newItem;

                CurrentItem = newItem;
            }

            public void Remove()
            {
                var previous = CurrentItem.Previous;
                var next = CurrentItem.Next;

                previous.Next = next;
                next.Previous = previous;

                CurrentItem = next;
            }

            public override string ToString()
            {
                if (CurrentItem.Next == CurrentItem)
                {
                    return CurrentValue.ToString();
                }

                var values = new List<int>();
                var loopItem = CurrentItem;

                do
                {
                    values.Add(loopItem.Value);
                    loopItem = loopItem.Next;
                } while (loopItem != CurrentItem);

                return string.Join(" - ", values);
            }

            private class Item
            {
                public Item(int value)
                {
                    Value = value;
                }

                public int Value { get; }

                public Item Previous { get; set; }
                public Item Next { get; set; }

                public override string ToString() => Value.ToString();
            }
        }
    }
}
