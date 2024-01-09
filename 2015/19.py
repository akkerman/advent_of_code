# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from utils import find_occurences
from collections import defaultdict


def part_one(replacements, molecule):
    """ part one """
    distinct = set()
    for f, to in replacements:
        for idx in find_occurences(f, molecule):
            distinct.add(molecule[:idx] + to + molecule[idx+len(f):])
    return len(distinct)


def part_two(molecule):
    """ part two """

    def lf(pattern):
        return len(find_occurences(pattern, molecule))

    # see https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/
    # for this formula
    return lf('[A-Z]') - lf('Rn') - lf('Ar') - 2 * lf('Y') - 1


def main():
    """ main """
    replacements = []
    molecule = ''
    for line in sys.stdin:
        line = line.replace('\n', '')

        if line == '':
            continue
        if '=>' not in line:
            molecule = line
            continue
        f, t = line.split(' => ')
        replacements.append((f, t))

    print('part_one', part_one(replacements, molecule))

    print('part_two', part_two(molecule))


main()
