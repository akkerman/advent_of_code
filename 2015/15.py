# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from functools import reduce

def measure(scoops, ingredient):
    return tuple(scoops*prop for prop in ingredient)

def score_cookie(cookie):
    total = [sum(ingredient) for ingredient in zip(*cookie)]
    return reduce(lambda a, b: a*max(0,b), total[:-1], 1)
    # Caveat, ignore calories which is last in total



def test():
    B = (-1, -2, 6, 3, 8)
    C = (2, 3, -2, -1, 3)

    return score_cookie([measure(44,B), measure(56, C)])

assert test() == 62842880


def part_one(lines):
    """ part one """

    best = 0
    for frosting in range(0,100):
        for candy in range(0,100):
            for bscotch in range(0,100):
                sugar = 100-frosting-candy-bscotch
                if sugar < 0:
                    continue

                score = score_cookie([
                      measure(frosting, lines[0]),
                      measure(candy, lines[1]),
                      measure(bscotch, lines[2]),
                      measure(sugar, lines[3]),
                ])


                best=max(best,score)

    return best



def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    reg = re.compile(r'.+ capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)')
    for line in sys.stdin:
        line = line.replace('\n', '')
        c,d,f,t,calories = reg.match(line).groups()
        lines.append((int(c),int(d),int(f),int(t),int(calories)))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
