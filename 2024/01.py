"""Day 1: Historian Hysteria."""
import sys
from collections import Counter


def part_one(list1: list[int], list2: list[int]):
    """Calculate total distance between two lists."""
    paired = zip(sorted(list1), sorted(list2))
    return sum(abs(one - two) for one, two in paired)

def part_two(list1: list[int], list2: list[int]):
    """Calculate simularity score between to lists."""
    counts = Counter(list2)
    return sum(one * counts[one] for one in list1)


def main():
    """Parse input file, pass to puzzle solvers."""
    list1: list[int] = []
    list2: list[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        one, two = line.split('   ')
        list1.append(int(one))
        list2.append(int(two))
        

    print('part_one', part_one(list1, list2))

    print('part_two', part_two(list1, list2))


main()
