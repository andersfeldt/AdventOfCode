using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day16 : IDay
    {
        private readonly IList<string> m_input;

        public Day16(string[] input) => m_input = input;

        public string SolveA()
        {
            const int numberOfOperations = 16;
            var candidates = Enumerable.Range(0, numberOfOperations)
                .ToDictionary(i => i, _ => GetAllOperations());

            var samples = CreateSamples();

            foreach (var sample in samples)
            {
                candidates[sample.Opcode] = candidates[sample.Opcode]
                    .Where(op => IsMatch(sample, op))
                    .ToList();
            }

            return samples
                .Count(sample => candidates[sample.Opcode].Count >= 3)
                .ToString();
        }

        private IList<Func<int[], int, int, int, int[]>> GetAllOperations()
        {
            return new List<Func<int[], int, int, int, int[]>>
            {
                Operations.Addr,
                Operations.Addi,

                Operations.Mulr,
                Operations.Muli,

                Operations.Banr,
                Operations.Bani,

                Operations.Borr,
                Operations.Bori,

                Operations.Setr,
                Operations.Seti,

                Operations.Gtir,
                Operations.Gtri,
                Operations.Gtrr,

                Operations.Eqir,
                Operations.Eqri,
                Operations.Eqrr
            };
        }

        private IList<Sample> CreateSamples()
        {
            return Create().ToList();

            IEnumerable<Sample> Create()
            {
                Sample sample = null;
                bool isComplete = false;
                int step = 0;

                foreach (var line in m_input)
                {
                    switch (step)
                    {
                        case 0:
                            if (string.IsNullOrWhiteSpace(line))
                            {
                                yield break;
                            }

                            sample = new Sample
                            {
                                Before = ParseArray(line)
                            };
                            break;

                        case 1:
                            var values = line.Split(' ')
                                .Select(int.Parse)
                                .ToArray();
                            sample.Opcode = values[0];
                            sample.A = values[1];
                            sample.B = values[2];
                            sample.C = values[3];
                            break;

                        case 2:
                            sample.After = ParseArray(line);
                            isComplete = true;
                            break;
                    }

                    step = (step + 1) % 4;

                    if (isComplete)
                    {
                        isComplete = false;
                        yield return sample;
                    }
                }
            }

            int[] ParseArray(string line)
            {
                var chars = line
                    .SkipWhile(c => c != '[')
                    .Skip(1)
                    .TakeWhile(c => c != ']')
                    .ToArray();

                return new string(chars)
                    .Split(',')
                    .Select(int.Parse)
                    .ToArray();
            }
        }

        private bool IsMatch(Sample sample, Func<int[], int, int, int, int[]> op)
        {
            var actual = op(sample.Before.ToArray(), sample.A, sample.B, sample.C);

            return sample.After.SequenceEqual(actual);
        }

        private class Sample
        {
            public int Opcode { get; set; }
            public int A { get; set; }
            public int B { get; set; }
            public int C { get; set; }
            public int[] Before { get; set; }
            public int[] After { get; set; }
        }

        private static class Operations
        {
            public static int[] Addr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] + registers[b];
                return registers;
            }

            public static int[] Addi(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] + b;
                return registers;
            }

            public static int[] Mulr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] * registers[b];
                return registers;
            }

            public static int[] Muli(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] * b;
                return registers;
            }

            public static int[] Banr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] & registers[b];
                return registers;
            }

            public static int[] Bani(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] & b;
                return registers;
            }

            public static int[] Borr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] | registers[b];
                return registers;
            }

            public static int[] Bori(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] | b;
                return registers;
            }

            public static int[] Setr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a];
                return registers;
            }

            public static int[] Seti(int[] registers, int a, int b, int c)
            {
                registers[c] = a;
                return registers;
            }

            public static int[] Gtir(int[] registers, int a, int b, int c)
            {
                registers[c] = a > registers[b] ? 1 : 0;
                return registers;
            }

            public static int[] Gtri(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] > b ? 1 : 0;
                return registers;
            }

            public static int[] Gtrr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] > registers[b] ? 1 : 0;
                return registers;
            }

            public static int[] Eqir(int[] registers, int a, int b, int c)
            {
                registers[c] = a == registers[b] ? 1 : 0;
                return registers;
            }

            public static int[] Eqri(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] == b ? 1 : 0;
                return registers;
            }

            public static int[] Eqrr(int[] registers, int a, int b, int c)
            {
                registers[c] = registers[a] == registers[b] ? 1 : 0;
                return registers;
            }
        }

        public string SolveB()
        {
            var operations = GetEstablishedOperations();
            var instructions = GetInstructions();
            var registers = new[] { 0, 0, 0, 0 };

            foreach (var instruction in instructions)
            {
                var opcode = instruction[0];
                var a = instruction[1];
                var b = instruction[2];
                var c = instruction[3];

                registers = operations[opcode](registers, a, b, c);
            }

            return registers[0].ToString();
        }

        private IDictionary<int, Func<int[], int, int, int, int[]>> GetEstablishedOperations()
        {
            const int numberOfOperations = 16;
            var candidates = Enumerable.Range(0, numberOfOperations)
                .ToDictionary(i => i, _ => GetAllOperations());

            var samples = CreateSamples();

            foreach (var sample in samples)
            {
                candidates[sample.Opcode] = candidates[sample.Opcode]
                    .Where(op => IsMatch(sample, op))
                    .ToList();
            }

            var establishedOps = new Dictionary<int, Func<int[], int, int, int, int[]>>();

            while (candidates.Any())
            {
                var candidate = candidates.First(c => c.Value.Count() == 1);
                var opcode = candidate.Key;
                var operation = candidate.Value.First();

                establishedOps[opcode] = operation;

                candidates.Remove(opcode);
                foreach (var kvp in candidates)
                {
                    kvp.Value.Remove(operation);
                }
            }

            return establishedOps;
        }

        private IList<int[]> GetInstructions()
        {
            return m_input
                .Skip(GetStartLineIndex())
                .Select(ParseArray)
                .ToList();

            int GetStartLineIndex()
            {
                for (var i = 2; i < m_input.Count; i++)
                {
                    if (string.IsNullOrWhiteSpace(m_input[i - 2]) && string.IsNullOrWhiteSpace(m_input[i - 1]) && string.IsNullOrWhiteSpace(m_input[i]))
                    {
                        return i + 1;
                    }
                }

                throw new Exception("Not expected...");
            }

            int[] ParseArray(string line)
                => line.Split(' ')
                    .Select(int.Parse)
                    .ToArray();
        }
    }
}
