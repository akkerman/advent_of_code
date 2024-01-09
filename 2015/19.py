# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from utils import find_occurences


def part_one(replacements, molecule):
    """ part one """
    distinct = set()
    for f, to in replacements:
        for idx in find_occurences(f, molecule):
            distinct.add(molecule[:idx] + to + molecule[idx+len(f):])
    return len(distinct)


def part_two(replacements, molecule):
    """ part two """
    return 'todo'


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

    print('part_two', part_two(replacements, molecule))


main()
