# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
import re
from collections import Counter


def part_one(foods):
    """ part one """
    allergen_count = Counter()
    ingredient_count = Counter()

    for ingredients,_ in foods:
        ingredient_count.update(ingredients)

    for _, allergens in foods:
        allergen_count.update(allergens)

    no_allergens = set(ingredient_count.keys())

    for allergen, _ in allergen_count.most_common():
        ingredient_sets = [set(ing) for (ing, allergens) in foods if allergen in allergens]
        over = set.intersection(*ingredient_sets)
        no_allergens -= over

    print(no_allergens)

    count = 0
    for ingredient in no_allergens:
        print(ingredient, ingredient_count[ingredient])
        count += ingredient_count[ingredient]

    return count




def part_two(foods):
    """ part two """

    return "todo"


def main():
    """ main """
    foods = []
    regex = re.compile('(.+) \\(contains (.+)\\)')
    for line in sys.stdin:
        line = line.replace('\n', '')
        m = regex.match(line)

        if not m:
            assert False

        ingredients,allergens = m.groups()
        ingredients=ingredients.split(' ')
        allergens=allergens.split(', ')

        foods.append((ingredients, allergens))

    # too low 199
    print('part_one', part_one(foods))

    print('part_two', part_two(foods))


main()
