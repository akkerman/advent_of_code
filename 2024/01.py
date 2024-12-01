# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List
from collections import Counter


def part_one(list1: List[int], list2: List[int]):
    """ part one """
    paired = zip(sorted(list1), sorted(list2))

    sum = 0
    for one, two in paired:
        sum += abs(one - two)

    return sum


def part_two(list1: List[int], list2: List[int]):
    """ part two """

    counts = Counter(list2)

    sum = 0
    for one in list1:
        similarity = one * counts[one]
        sum = sum + similarity

    return sum


def main():
    """ main """
    list1: List[int] = []
    list2: List[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        one, two = line.split('   ')
        list1.append(int(one))
        list2.append(int(two))
        

    print('part_one', part_one(list1, list2))

    print('part_two', part_two(list1, list2))


main()
