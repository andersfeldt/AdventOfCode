using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day08 : IDay
    {
        private readonly IList<int> m_input;

        public Day08(string[] input)
            => m_input = input
                .Single()
                .Split(' ')
                .Select(int.Parse)
                .ToList();

        public string SolveA()
        {
            var queue = new Queue<int>(m_input);
            var metadataSum = CalculateMetadataSum(queue);

            return metadataSum.ToString();
        }

        private int CalculateMetadataSum(Queue<int> queue)
        {
            var numberOfChildren = queue.Dequeue();
            var numberOfMetadataEntries = queue.Dequeue();
            var metadataSum = 0;

            for (var i = 0; i < numberOfChildren; i++)
            {
                metadataSum += CalculateMetadataSum(queue);
            }

            for (var i = 0; i < numberOfMetadataEntries; i++)
            {
                metadataSum += queue.Dequeue();
            }

            return metadataSum;
        }

        public string SolveB()
        {
            var queue = new Queue<int>(m_input);
            var nodeValue = CalculateNodeValue(queue);

            return nodeValue.ToString();
        }

        private int CalculateNodeValue(Queue<int> queue)
        {
            var numberOfChildren = queue.Dequeue();
            var numberOfMetadataEntries = queue.Dequeue();
            var nodeValue = 0;

            if (numberOfChildren == 0)
            {
                for (var i = 0; i < numberOfMetadataEntries; i++)
                {
                    nodeValue += queue.Dequeue();
                }
            }
            else
            {
                var childValues = new Dictionary<int, int>();

                for (var i = 1; i <= numberOfChildren; i++)
                {
                    childValues[i] = CalculateNodeValue(queue);
                }

                var metadataEntries = new List<int>();

                for (var i = 0; i < numberOfMetadataEntries; i++)
                {
                    var metadataEntry = queue.Dequeue();

                    if (childValues.TryGetValue(metadataEntry, out var childValue))
                    {
                        nodeValue += childValue;
                    }
                }
            }

            return nodeValue;
        }
    }
}
