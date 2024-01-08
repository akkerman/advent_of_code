# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from itertools import combinations

def part_one(containers, target):
    """ part one """
    count = 0
    for r in range(len(containers)):
        for comb in combinations(containers, r):
            if sum(comb) == target:
                count+=1
    return count


def part_two(containers, target):
    """ part two """

    # find the minimum amount of containers that hold 150L
    min_amount = len(containers)+1
    for r in range(len(containers)):
        for comb in combinations(containers, r):
            if sum(comb) == target:
                min_amount=min(min_amount,len(comb))


    # find the number of combinations of length min_amount holding 150L
    num_comb = 0
    for comb in combinations(containers, min_amount):
        if sum(comb) == target:
            num_comb += 1
    return num_comb


def main():
    """ main """
    containers = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        containers.append(int(line))

    target = 150 if len(containers) > 5 else 25

    print('part_one', part_one(containers, target))
    print('part_two', part_two(containers, target))


main()
