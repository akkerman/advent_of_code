# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

MFCSAM = [
    ['children', 'children: 3', set()],
    ['cats', 'cats: 7', set()],
    ['samoyeds', 'samoyeds: 2', set()],
    ['pomeranians', 'pomeranians: 3', set()],
    ['akitas', 'akitas: 0', set()],
    ['vizslas', 'vizslas: 0', set()],
    ['goldfish', 'goldfish: 5', set()],
    ['trees', 'trees: 3', set()],
    ['cars', 'cars: 2', set()],
    ['perfumes', 'perfumes: 1', set()],
]


def part_one(lines):
    """ part one """
    sue = set()
    for line in lines:
        sue.add(line)
        for excl, incl, s in MFCSAM:
            if excl not in line or incl in line:
                s.add(line)

    for _, _, s in MFCSAM:
        sue = sue & s


    return sue



def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
