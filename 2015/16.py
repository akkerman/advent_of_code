# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

MFCSAM = [
    ['children', 'children: 3',  3, set()],
    ['cats', 'cats: 7',  7, set()],
    ['samoyeds', 'samoyeds: 2',  2, set()],
    ['pomeranians', 'pomeranians: 3',  3, set()],
    ['akitas', 'akitas: 0',  0, set()],
    ['vizslas', 'vizslas: 0',  0, set()],
    ['goldfish', 'goldfish: 5',  5, set()],
    ['trees', 'trees: 3',  3, set()],
    ['cars', 'cars: 2',  2, set()],
    ['perfumes', 'perfumes: 1',  1, set()],
]

def part_one(lines):
    """ part one """
    sue = set()
    for line in lines:
        sue.add(line)
        for excl, incl, _, s in MFCSAM:
            if excl not in line or incl in line:
                s.add(line)

    for _, _, _, s in MFCSAM:
        sue = sue & s

    return sue



def part_two(lines):
    """ part two """
    gt = ['cats', 'trees']
    lt = ['pomeranians', 'goldfish']
    special = gt + lt

    sue = set()
    for line in lines:
        sue.add(line)
        for compound, incl, n, s in MFCSAM:
            if compound not in line:
                s.add(line)
                continue

            if compound not in special:
                if incl in line:
                    s.add(line)
                continue

            count = int(re.match('.*'+compound+': (\d+).*', line).groups()[0])

            if compound in lt:
                if count < n:
                    s.add(line)
                continue

            if compound in gt:
                if count > n:
                    s.add(line)
                continue

    for _, _, _, s in MFCSAM:
        sue = sue & s

    return sue


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
