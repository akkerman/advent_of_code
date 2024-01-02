# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
import re
from collections import Counter, deque


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

    count = 0
    for ingredient in no_allergens:
        count += ingredient_count[ingredient]

    return count


def part_two(foods):
    """ part two """
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

    allergen2ingredient = dict()
    candidates = ingredient_count.keys() - no_allergens
    queue = deque(allergen for (allergen, _) in allergen_count.most_common())

    while len(queue):
        allergen = queue.popleft()
        ingredient_sets = [set(ing) for (ing, allergens) in foods if allergen in allergens]
        over = set.intersection(*ingredient_sets)
        over = over - (over - candidates)
        if len(over) == 1:
            i = over.pop()
            allergen2ingredient[allergen] = i
            candidates.remove(i)
        else:
            queue.append(allergen)


    

    return ",".join([allergen2ingredient[key] for key in sorted(allergen2ingredient)])


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
