using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day11 : IDay
    {
        private readonly int m_input;

        public Day11(string[] input) => m_input = int.Parse(input.Single());

        public string SolveA()
        {
            const int gridSize = 300;
            const int subSquareSize = 3;

            var grid = CreateGrid(gridSize, serialNumber: m_input);
            var (x, y) = GetSubSquareWithLargestTotalPower(grid, gridSize, subSquareSize);

            return $"{x},{y}";
        }

        private IDictionary<(int, int), int> CreateGrid(int size, int serialNumber)
        {
            return GetAllPositions(1, 1, size)
                .ToDictionary(p => p, p => CalculatePowerLevel(p.X, p.Y));

            int CalculatePowerLevel(int x, int y)
            {
                var rackId = x + 10;
                var powerLevel = (rackId * y + serialNumber) * rackId;

                return GetHundredsDigit(powerLevel) - 5;
            }

            int GetHundredsDigit(int i) => (i % 1000) / 100;
        }

        private (int X, int Y) GetSubSquareWithLargestTotalPower(IDictionary<(int, int), int> grid, int gridSize, int subSquareSize)
        {
            var totalPowerInSubSquares = GetAllPositions(1, 1, gridSize - subSquareSize)
                .ToDictionary(p => p, p => GetTotalPower(p.X, p.Y));

            var max = totalPowerInSubSquares.Values.Max();

            return totalPowerInSubSquares.First(kvp => kvp.Value == max).Key;

            int GetTotalPower(int startX, int startY)
            {
                return GetAllPositions(startX, startY, subSquareSize)
                    .Select(p => grid[p])
                    .Sum();
            }
        }

        private IEnumerable<(int X, int Y)> GetAllPositions(int startX, int startY, int size)
        {
            for (var x = startX; x < startX + size; x++)
            {
                for (var y = startY; y < startY + size; y++)
                {
                    yield return (x, y);
                }
            }
        }

        public string SolveB()
        {
            const int gridSize = 300;

            var grid = CreateGrid(gridSize, serialNumber: m_input);

            var largestTotalPowerFound = 0;
            var largestSquareId = string.Empty;

            foreach (var (startX, startY) in GetAllPositions(1, 1, gridSize))
            {
                var currentSubGridPower = 0;
                var maxSubGridSize = gridSize + 1 - Math.Max(startX, startY);

                for (var size = 1; size <= maxSubGridSize; size++)
                {
                    var sum = GetEdgePositions(startX, startY, size)
                        .Select(p => grid[p])
                        .Sum();
                    currentSubGridPower += sum;

                    if (currentSubGridPower > largestTotalPowerFound)
                    {
                        largestTotalPowerFound = currentSubGridPower;
                        largestSquareId = $"{startX},{startY},{size}";
                    }
                }
            }

            return largestSquareId;
        }

        private IEnumerable<(int X, int Y)> GetEdgePositions(int startX, int startY, int size)
        {
            var endX = startX + size - 1;
            var endY = startY + size - 1;

            var horizontal = Enumerable
                .Range(startX, size)
                .Select(x => (x, endY));

            var vertical = Enumerable
                .Range(startY, size - 1)
                .Select(y => (endX, y));

            return horizontal.Concat(vertical);
        }
    }
}
