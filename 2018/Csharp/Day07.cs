using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode.Y2018.Csharp
{
    public class Day07 : IDay
    {
        private readonly IList<string> m_input;

        public Day07(string[] input) => m_input = input;

        public string SolveA()
        {
            var remainingRules = BuildRules();
            var completedSteps = new List<char>();

            while (remainingRules.Any())
            {
                var nextStep = remainingRules
                    .Where(kvp => !kvp.Value.Any())
                    .OrderBy(kvp => kvp.Key)
                    .First()
                    .Key;

                completedSteps.Add(nextStep);
                remainingRules.Remove(nextStep);

                foreach (var kvp in remainingRules)
                {
                    kvp.Value.Remove(nextStep);
                }
            }

            return new string(completedSteps.ToArray());
        }

        private IDictionary<char, List<char>> BuildRules()
        {
            var rules = new Dictionary<char, List<char>>();

            foreach (var str in m_input)
            {
                var stepMustBeFinished = str[5];
                var beforeStepCanBegin = str[36];

                if (rules.ContainsKey(beforeStepCanBegin))
                {
                    rules[beforeStepCanBegin].Add(stepMustBeFinished);
                }
                else
                {
                    rules[beforeStepCanBegin] = new List<char> { stepMustBeFinished };
                }

                if (!rules.ContainsKey(stepMustBeFinished))
                {
                    rules[stepMustBeFinished] = new List<char>();
                }
            }

            return rules;
        }

        public string SolveB()
        {
            const int NumberOfWorkers = 5;

            var workers = Enumerable.Range(0, NumberOfWorkers)
                .Select(_ => new Worker())
                .ToList();

            var remainingRules = BuildRules();
            var elapsedTime = 0;

            while (remainingRules.Any() || workers.Any(w => !w.IsIdle))
            {
                var idleWorkers = workers
                    .Where(w => w.IsIdle)
                    .ToList();

                var nextSteps = remainingRules
                    .Where(kvp => !kvp.Value.Any())
                    .Select(kvp => kvp.Key)
                    .OrderBy(c => c)
                    .Take(idleWorkers.Count)
                    .ToList();

                for (var i = 0; i < nextSteps.Count; i++)
                {
                    var step = nextSteps[i];
                    idleWorkers[i].Accept(step);
                    remainingRules.Remove(step);
                }

                foreach (var worker in workers)
                {
                    worker.DoWork();
                }

                var completedSteps = workers
                    .Select(w => w.GetCompletedStepOrDefault())
                    .Where(x => x != null)
                    .Select(x => x.Value)
                    .ToList();

                foreach (var step in completedSteps)
                {
                    foreach (var kvp in remainingRules)
                    {
                        kvp.Value.Remove(step);
                    }
                }

                elapsedTime++;
            }

            return elapsedTime.ToString();
        }

        private class Worker
        {
            private char? m_currentStep;
            private int m_remainingTime;

            public bool IsIdle => m_currentStep == null;

            public void Accept(char step)
            {
                m_currentStep = step;
                m_remainingTime = GetInitialTime(step);
            }

            private static int GetInitialTime(char c) => c - 'A' + 61;

            public void DoWork()
            {
                if (IsIdle)
                {
                    return;
                }

                m_remainingTime--;
            }

            public char? GetCompletedStepOrDefault()
            {
                if (IsIdle || m_remainingTime > 0)
                {
                    return null;
                }

                try
                {
                    return m_currentStep;
                }
                finally
                {
                    m_currentStep = null;
                }
            }
        }
    }
}
