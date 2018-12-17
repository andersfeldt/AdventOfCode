using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day13 : IDay
    {
        private readonly IList<string> m_input;

        public Day13(string[] input) => m_input = input;

        public string SolveA()
        {
            var (map, carts) = CreateMapAndCarts();

            while (true)
            {
                var orderedCarts = carts
                    .OrderBy(c => c.Y)
                    .ThenBy(c => c.X)
                    .ToList();

                foreach (var cart in orderedCarts)
                {
                    cart.Move(map);

                    var collision = GetCollisionOrDefault(carts);
                    if (collision != null)
                    {
                        return collision;
                    }
                }
            }
        }

        private (IDictionary<(int, int), char>, IList<Cart>) CreateMapAndCarts()
        {
            var map = new Dictionary<(int, int), char>();
            var carts = new List<Cart>();

            for (var y = 0; y < m_input.Count; y++)
            {
                var line = m_input[y];
                for (var x = 0; x < line.Length; x++)
                {
                    var c = line[x];

                    switch (c)
                    {
                        case '-': case '|': case '+': case '/': case '\\': map[(x, y)] = c; break;
                        case '^': map[(x, y)] = '|'; carts.Add(new Cart(x, y, c)); break;
                        case 'v': map[(x, y)] = '|'; carts.Add(new Cart(x, y, c)); break;
                        case '<': map[(x, y)] = '-'; carts.Add(new Cart(x, y, c)); break;
                        case '>': map[(x, y)] = '-'; carts.Add(new Cart(x, y, c)); break;
                    }
                }
            }

            return (map, carts);
        }

        private string GetCollisionOrDefault(IList<Cart> carts)
            => carts
                .GroupBy(c => $"{c.X},{c.Y}")
                .Where(grouping => grouping.Count() > 1)
                .Select(grouping => grouping.Key)
                .FirstOrDefault();

        private class Cart
        {
            public Cart(int x, int y, char direction)
            {
                X = x;
                Y = y;
                Direction = direction;
                TurningMode = TurningMode.Left;
            }

            public int X { get; set; }
            public int Y { get; set; }
            public char Direction { get; set; }
            public TurningMode TurningMode { get; set; }

            public void Move(IDictionary<(int, int), char> map)
            {
                var currentTrack = map[(X, Y)];

                switch ($"{Direction}{currentTrack}")
                {
                    case ">-": case "^/": case @"v\": MoveEast(); return;
                    case "<-": case "v/": case @"^\": MoveWest(); return;
                    case "v|": case "</": case @">\": MoveSouth(); return;
                    case "^|": case ">/": case @"<\": MoveNorth(); return;
                }

                if (currentTrack != '+')
                {
                    throw new Exception($"Unexpected state: '{currentTrack}'");
                }

                switch ($"{Direction} {TurningMode}")
                {
                    case "> Straight": case "v Left": case "^ Right": MoveEast(); break;
                    case "< Straight": case "^ Left": case "v Right": MoveWest(); break;
                    case "> Right": case "< Left": case "v Straight": MoveSouth(); break;
                    case "> Left": case "< Right": case "^ Straight": MoveNorth(); break;
                }

                SetNextTurningMode();

                void MoveEast() { X++; Direction = '>'; }
                void MoveWest() { X--; Direction = '<'; }
                void MoveSouth() { Y++; Direction = 'v'; }
                void MoveNorth() { Y--; Direction = '^'; }
                void SetNextTurningMode() => TurningMode = (TurningMode)(((int)TurningMode + 1) % 3);
            }
        }

        private enum TurningMode
        {
            Left,
            Straight,
            Right
        }

        public string SolveB()
        {
            var (map, carts) = CreateMapAndCarts();

            while (true)
            {
                carts = carts
                    .OrderBy(c => c.Y)
                    .ThenBy(c => c.X)
                    .ToList();

                var collidingCartIndices = new List<int>();

                for (var i = 0; i < carts.Count; i++)
                {
                    if (collidingCartIndices.Contains(i))
                    {
                        continue;
                    }

                    carts[i].Move(map);

                    collidingCartIndices.AddRange(GetCollidingCarts(carts).Select(carts.IndexOf));
                }

                foreach (var index in collidingCartIndices.Distinct().OrderByDescending(i => i))
                {
                    carts.RemoveAt(index);
                }

                if (carts.Count == 1)
                {
                    var cart = carts[0];
                    return $"{cart.X},{cart.Y}";
                }
            }
        }

        private IList<Cart> GetCollidingCarts(IList<Cart> carts)
            => carts
                .GroupBy(c => $"{c.X},{c.Y}")
                .Where(grouping => grouping.Count() > 1)
                .SelectMany(x => x)
                .ToList();
    }
}
