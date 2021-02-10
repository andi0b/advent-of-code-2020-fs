using System;
using System.Collections.Generic;
using System.Linq;

namespace aoc_runner
{
    public class Day22
    {        
        public int Part1(string input)
        {
            var decks = Parse(input);

            while (!decks.Any(x => x.IsEmpty))
            {
                var ordered = decks.OrderBy(x => x.Top).ToArray();
                var winner = ordered.Last();
                var loser = ordered.First();

                winner.Put(winner.Take(), loser.Take());
            }

            return decks.Single(x => !x.IsEmpty).CalculateScore();
        }

        public int Part2(string input)
        {
            var decks = Parse(input);
            var game = new Game(decks);
            var winner = game.Play();
            return decks[winner].CalculateScore();
        }
        
        private record Game(PlayerDeck[] Decks, int GameId = 1)
        {
            private readonly HashSet<(int, int)> _previousStates = new();
            
            public int Play()
            {
                while(true)
                {
                    var playedCards = Decks.Select(x => x.Top).ToArray();

                    // player 1 wins on recursion
                    if (!_previousStates.Add((Decks[0].CalculateScore(), Decks[1].CalculateScore()))) 
                        return 0;

                    var isRecursiveCombat = Decks.All(deck => deck.Remaining - 1 >= deck.Top);

                    var winnerId = isRecursiveCombat
                        ? new Game(Decks.Select(deck => deck.SubDeck()).ToArray(), GameId + 1).Play()
                        : playedCards.Select((card, playerId) => (card, playerId)).OrderByDescending(x => x.card).First().playerId;

                    Decks[winnerId].Put(Decks[winnerId].Take(), Decks[winnerId == 0 ? 1 : 0].Take());

                    if (Decks[0].IsEmpty) return 1;
                    if (Decks[1].IsEmpty) return 0;
                }
            } 
        }
        
        
        public PlayerDeck[] Parse(string input)
        {
            PlayerDeck ParseDeck(string i) => new(i.Split(Environment.NewLine).Skip(1).Select(int.Parse));
                
            var playerParts = input.Split(Environment.NewLine + Environment.NewLine);
            return new[]
            {
                ParseDeck(playerParts[0]),
                ParseDeck(playerParts[1])
            };
        }

        public class PlayerDeck
        {
            private readonly Queue<int> _cards;

            public PlayerDeck(IEnumerable<int> cards) => _cards = new(cards);

            public int Take() => _cards.Dequeue();

            public void Put(int winner, int loser)
            {
                _cards.Enqueue(winner);
                _cards.Enqueue(loser);
            }

            public int Top => _cards.TryPeek(out var card) ? card : -1;
            
            public bool IsEmpty => !_cards.TryPeek(out _);

            public int Remaining => _cards.Count;

            public int CalculateScore() => _cards.Reverse().Select((card, n) => card * (n+1)).Sum();

            public PlayerDeck SubDeck() => new(_cards.Skip(1).Take(_cards.First()));
        }
    }


}