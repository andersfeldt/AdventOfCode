using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day10 : IDay
    {
        private readonly IList<string> m_input;

        public Day10(string[] input) => m_input = input;

        public string SolveA()
        {
            var points = m_input
                .Select(Point.Parse)
                .ToList();

            PrintPoints(points);

            var messageFound = false;
            while (!messageFound)
            {
                points = points.Select(p => p.NextSecond()).ToList();
                messageFound = PrintPoints(points);
            }

            return null;
        }

        private bool PrintPoints(IList<Point> points)
        {
            const int limit = 64; //Found by experimentation...

            var minX = points.Select(p => p.PositionX).Min();
            var maxX = points.Select(p => p.PositionX).Max();
            var minY = points.Select(p => p.PositionY).Min();
            var maxY = points.Select(p => p.PositionY).Max();

            if (maxX - minX > limit || maxY - minY > limit)
            {
                return false;
            }

            Console.WriteLine(new string('-', maxX - minX + 3));
            for (var y = minY; y <= maxY; y++)
            {
                Console.Write('|');
                for (var x = minX; x <= maxX; x++)
                {
                    Console.Write(points.Any(p => p.PositionX == x && p.PositionY == y)
                        ? '#'
                        : ' ');
                }
                Console.WriteLine('|');
            }
            Console.WriteLine(new string('-', maxX - minX + 3));

            return true;
        }

        private class Point
        {
            public Point(int positionX, int positionY, int velocityX, int velocityY)
            {
                PositionX = positionX;
                PositionY = positionY;
                VelocityX = velocityX;
                VelocityY = velocityY;
            }

            public int PositionX { get; }
            public int PositionY { get; }
            public int VelocityX { get; }
            public int VelocityY { get; }

            public static Point Parse(string str)
            {
                var parts = str.Split('<', ',', '>');

                var positionX = int.Parse(parts[1]);
                var positionY = int.Parse(parts[2]);
                var velocityX = int.Parse(parts[4]);
                var velocityY = int.Parse(parts[5]);

                return new Point(positionX, positionY, velocityX, velocityY);
            }

            public Point NextSecond() => new Point(PositionX + VelocityX, PositionY + VelocityY, VelocityX, VelocityY);

            public override string ToString()
            {
                return $"position=<{PositionX}, {PositionY}> velocity=<{VelocityX}, {VelocityY}>";
            }
        }

        public string SolveB()
        {
            var points = m_input
                .Select(Point.Parse)
                .ToList();

            var time = 0;
            var messageFound = IsMessageFound(points);

            while (!messageFound)
            {
                time++;

                points = points.Select(p => p.NextSecond()).ToList();
                messageFound = IsMessageFound(points);
            }

            return time.ToString();
        }

        private bool IsMessageFound(IList<Point> points)
        {
            const int limit = 64; //Found by experimentation...

            var minX = points.Select(p => p.PositionX).Min();
            var maxX = points.Select(p => p.PositionX).Max();
            var minY = points.Select(p => p.PositionY).Min();
            var maxY = points.Select(p => p.PositionY).Max();

            return maxX - minX < limit && maxY - minY < limit;
        }
    }
}
