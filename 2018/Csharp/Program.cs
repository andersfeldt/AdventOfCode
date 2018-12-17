using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace AdventOfCode.Y2018.Csharp
{
    public class Program
    {
        private static Regex ArgsRegex = new Regex(@"(\d*)\s?([ab]?)", RegexOptions.IgnoreCase);

        public static void Main(string[] args)
        {
            var (dayNumber, problem) = ParseArgs(args);
            Solve(dayNumber, problem);
        }

        private static (int, Problem) ParseArgs(string[] args)
        {
            var match = ArgsRegex.Match(string.Join(string.Empty, args));

            var dayNumber = ParseDayNumber(match.Groups[1].Value);
            var problem = ParseProblem(match.Groups[2].Value);

            return (dayNumber, problem);

            int ParseDayNumber(string matchedValue)
                => int.TryParse(matchedValue, out var value)
                    ? value
                    : DateTime.Today.Day;

            Problem ParseProblem(string matchedValue)
                => matchedValue.ToUpper() == "B"
                    ? Problem.B
                    : Problem.A;
        }

        private static void Solve(int dayNumber, Problem problem)
        {
            Console.WriteLine($"Solving problem {dayNumber}{problem}");

            var content = File.ReadAllLines(GetPath(dayNumber));
            var day = GetDay();

            var startTime = DateTime.Now;

            var answer = problem == Problem.A
                ? day.SolveA()
                : day.SolveB();

            var endTime = DateTime.Now;

            Console.WriteLine(answer);
            Console.WriteLine($"(Elapsed time: {endTime - startTime})");

            IDay GetDay()
            {
                switch (dayNumber)
                {
                    case 1: return new Day01(content); //summing frequencies
                    case 2: return new Day02(content); //id checksums
                    case 3: return new Day03(content); //overlapping squares
                    case 4: return new Day04(content); //sleeping guards
                    case 5: return new Day05(content); //collapsing polymer
                    case 6: return new Day06(content); //Manhattan distances and largest areas
                    case 7: return new Day07(content); //ordered instructions (DAG)
                    case 8: return new Day08(content); //nodes with metadata
                    case 9: return new Day09(content); //marbles in circular list
                    case 10: return new Day10(content); //message in the sky
                    case 11: return new Day11(content); //fuel cell grid
                    case 12: return new Day12(content); //pots and plants
                    case 13: return new Day13(content); //colliding carts
                    case 14: return new Day14(content); //chocolate recipes

                    default:
                        throw new NotImplementedException();
                }
            }
        }

        private static string GetPath(int dayNumber)
        {
            var fileName = $"Day{dayNumber:00}.txt";
            return Path.Combine(Environment.CurrentDirectory, "..", "PuzzleInputs", fileName);
        }

        private enum Problem
        {
            A, B
        }
    }
}
