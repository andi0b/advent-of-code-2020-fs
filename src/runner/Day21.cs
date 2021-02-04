using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

namespace aoc_runner
{
    public record Day21(Day21.Food[] Foods)
    {
        public Day21(string[] input) : this(input.Select(Food.Parse).ToArray()) 
        {
        }


        public record Food(string[] Ingredients, string[] Allergens)
        {
            public static Food Parse(string input)
            {
                var match = Regex.Match(input, @"(?<ingredients>.*) \(contains (?<allergens>.*)\)");
                return new(
                    match.Groups["ingredients"].Value.Split(' '),
                    match.Groups["allergens"].Value.Split(", "));
            }
        }

        public int Part1() => Solve().part1;
        
        public string Part2() => Solve().part2;
        
        private (int part1, string part2) Solve()
        {
            var allergensPossibleIngredientsLookup = (
                from food in Foods
                from allergen in food.Allergens
                select (allergen, food.Ingredients)
            ).ToLookup(x => x.allergen, x => x.Ingredients);

            var ingredientsByAllergens =
                allergensPossibleIngredientsLookup.ToDictionary(
                    x => x.Key,
                    x => x.Aggregate(x.SelectMany(y => y),
                                     (acc, next) => acc.Intersect(next)).ToArray());

            KeyValuePair<string,string[]>[] toResolve;
            do
            {
                toResolve = ingredientsByAllergens.Where(x => x.Value.Length > 1).ToArray();
                var resolved = ingredientsByAllergens.Values.Where(x => x.Length == 1).Select(x => x.Single()).ToArray();

                foreach (var (key, value) in toResolve)
                {
                    ingredientsByAllergens[key] = value.Except(resolved).ToArray();
                }
                
            } while (toResolve.Any());
            
            var part2 = string.Join(",", ingredientsByAllergens.OrderBy(x=>x.Key).Select(x => x.Value.Single()));

            
            var allIngredients = Foods.SelectMany(x => x.Ingredients);
            var allAllergenicIngredients = ingredientsByAllergens.Values.SelectMany(x => x);
            var allNonAllergenicIngredients = allIngredients.Where(x => !allAllergenicIngredients.Contains(x));
            var part1 = allNonAllergenicIngredients.Count();

            return (part1, part2);
        }
    }
}