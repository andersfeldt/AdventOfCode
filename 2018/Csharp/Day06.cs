using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day06 : IDay
    {
        private readonly IList<string> m_input;

        public Day06(string[] input) => m_input = input;

        public string SolveA()
        {
            var riskPositions = m_input
                .Select(s => new Position(s))
                .ToList();

            var (topLeft, bottomRight) = GetGridRange(riskPositions);

            var grid = GetGridPositions(topLeft, bottomRight)
                .ToDictionary(pos => pos, _ => new Proximity());

            for (var riskPositionIndex = 0; riskPositionIndex < riskPositions.Count; riskPositionIndex++)
            {
                foreach (var kvp in grid)
                {
                    var gridPosition = kvp.Key;
                    var proximity = kvp.Value;
                    var distance = riskPositions[riskPositionIndex].GetDistance(gridPosition);

                    if (proximity.TargetIds == null || distance < proximity.Distance)
                    {
                        proximity.TargetIds = new List<int> { riskPositionIndex };
                        proximity.Distance = distance;
                    }
                    else if (distance == proximity.Distance)
                    {
                        proximity.TargetIds.Add(riskPositionIndex);
                    }
                }
            }

            var riskPositionTargetIdsValidForAreaCount = riskPositions
                .Select((position, index) => new { Position = position, TargetId = index })
                .Where(x => x.Position.IsInsideRange(topLeft, bottomRight))
                .Select(x => x.TargetId)
                .ToList();

            var largestAreaSize = grid
                .Where(kvp => kvp.Value.TargetIds.Count == 1 && riskPositionTargetIdsValidForAreaCount.Contains(kvp.Value.TargetIds[0]))
                .Select(kvp => kvp.Value)
                .GroupBy(prox => prox.TargetIds[0])
                .Select(grouping => grouping.Count())
                .Max();

            return largestAreaSize.ToString();
        }

        private (Position, Position) GetGridRange(IList<Position> positions)
        {
            var minX = positions.Select(p => p.X).Min();
            var maxX = positions.Select(p => p.X).Max();
            var minY = positions.Select(p => p.Y).Min();
            var maxY = positions.Select(p => p.Y).Max();

            return (new Position(minX, minY), new Position(maxX, maxY));
        }

        private IEnumerable<Position> GetGridPositions(Position topLeft, Position bottomRight)
        {
            for (var x = topLeft.X; x <= bottomRight.X; x++)
            {
                for (var y = topLeft.Y; y <= bottomRight.Y; y++)
                {
                    yield return new Position(x, y);
                }
            }
        }

        private class Position
        {
            public Position(int x, int y)
            {
                X = x;
                Y = y;
            }

            public Position(string str)
            {
                var numbers = str.Split(',');
                X = int.Parse(numbers[0]);
                Y = int.Parse(numbers[1]);
            }

            public int X { get; set; }
            public int Y { get; set; }

            public int GetDistance(Position other)
                => Math.Abs(X - other.X) + Math.Abs(Y - other.Y);

            public bool IsInsideRange(Position topLeft, Position bottomRight)
                => topLeft.X < X && X < bottomRight.X
                && topLeft.Y < Y && Y < bottomRight.Y;
        }

        private class Proximity
        {
            public IList<int> TargetIds { get; set; }
            public int Distance { get; set; }
        }

        public string SolveB()
        {
            var safePositions = m_input
                .Select(s => new Position(s))
                .ToList();

            var (topLeft, bottomRight) = GetGridRange(safePositions);

            var grid = GetGridPositions(topLeft, bottomRight)
                .ToDictionary(pos => pos, _ => 0);

            for (var safePositionIndex = 0; safePositionIndex < safePositions.Count; safePositionIndex++)
            {
                foreach (var gridPosition in grid.Keys.ToList())
                {
                    grid[gridPosition] += safePositions[safePositionIndex].GetDistance(gridPosition);
                }
            }

            const int distanceLimit = 10000;

            return grid
                .Count(kvp => kvp.Value < distanceLimit)
                .ToString();
        }
    }
}
