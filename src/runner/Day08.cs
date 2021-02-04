using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public record Day08(Program Program)
    {
        public Day08(string[] input) : this(Program.Parse(input)) { }

        public int Part1() => new GameConsoleState(Program).NextStepsWithInfiniteLoopDetection().Last().Accumulator;

        public int Part2()
        {
            var programPermutations =
                from x in Program.Statements.Select((statement, idx) => (statement, idx))
                where x.statement.Op is Op.Jmp or Op.Nop
                let newStatement = x.statement.Op switch
                {
                    Op.Jmp => x.statement with {Op = Op.Nop},
                    Op.Nop => x.statement with {Op = Op.Jmp},
                }
                select Program.ReplaceStatementAt(x.idx, newStatement);

            var endStates =
                from program in programPermutations
                select new GameConsoleState(program).NextStepsWithInfiniteLoopDetection().Last();

            return endStates.Single(x => x.IsTerminated).Accumulator;
        }
    }

    public record GameConsoleState(Program Program, int StatementPointer = 0, int Accumulator = 0)
    {
        public Statement CurrentStatement => Program.Statements[StatementPointer];

        public bool IsTerminated => StatementPointer >= Program.Statements.Length;

        public GameConsoleState NextStep() => CurrentStatement switch
        {
            (Op.Acc, var param) => this with {
                StatementPointer = StatementPointer + 1,
                Accumulator = Accumulator + param
                },

            (Op.Nop, _) => this with {StatementPointer = StatementPointer + 1},

            (Op.Jmp, var param) => this with {StatementPointer = StatementPointer + param},
        };

        public IEnumerable<GameConsoleState> NextSteps()
        {
            for (var state = NextStep();; state = state.NextStep())
            {
                yield return state;
                if (state.IsTerminated) yield break;
            }
        }

        public IEnumerable<GameConsoleState> NextStepsWithInfiniteLoopDetection()
        {
            var executedStatementsIds = new HashSet<int>();
            return NextSteps()
               .TakeWhile(x => executedStatementsIds.Add(x.StatementPointer));
        }
    }

    public record Program(Statement[] Statements)
    {
        public static Program Parse(string[] input) => new(input.Select(Statement.Parse).ToArray());

        public Program ReplaceStatementAt(int index, Statement newStatement) => this with {
            Statements = Statements.Select((s, i) => i == index ? newStatement : s)
                                   .ToArray()
            };
    }

    public record Statement(Op Op, int Param)
    {
        public static Statement Parse(string input)
        {
            var parts = input.Split(' ');
            return new Statement(Enum.Parse<Op>(parts[0], ignoreCase: true), int.Parse(parts[1]));
        }
    }

    public enum Op
    {
        /// <summary>
        /// acc increases or decreases a single global value called the accumulator by the value given in the argument. For example,
        /// acc +7 would increase the accumulator by 7. The accumulator starts at 0. After an acc instruction, the instruction
        /// immediately below it is executed next.
        /// </summary>
        Acc,
        
        /// <summary>
        /// nop stands for No OPeration - it does nothing. The instruction immediately below it is executed next.
        /// </summary>
        Nop,
        
        /// <summary>
        /// jmp jumps to a new instruction relative to itself. The next instruction to execute is found using the argument as an
        /// offset from the jmp instruction; for example, jmp +2 would skip the next instruction, jmp +1 would continue to the instruction
        /// immediately below it, and jmp -20 would cause the instruction 20 lines above to be executed next.
        /// </summary>
        Jmp
    };
}