# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from itertools import combinations


def part_one(containers):
    """ part one """
    count = 0
    for r in range(len(containers)):
        for comb in combinations(containers, r):
            if sum(comb) == 150:
                count+=1
    return count


def part_two(containers):
    """ part two """
    return 'todo'


def main():
    """ main """
    containers = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        containers.append(int(line))

    print('part_one', part_one(containers))

    print('part_two', part_two(containers))


main()
