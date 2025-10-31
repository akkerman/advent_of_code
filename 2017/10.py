"""Day 10: Knot Hash."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache


def reverse_sub(lst: list[int], start:int, length:int):
    n = len(lst)
    for i in range(length // 2):
        s = (start + i) % n
        e = (start + length - 1 - i) % n
        lst[s], lst[e] = lst[e], lst[s]


def part_one(lst: list[int], lengths: list[int]):
    """Solution to part one."""
    pos = 0
    skip = 0
    n = len(lst)

    for lng in lengths:
        reverse_sub(lst, pos, lng)
        pos = (pos + lng + skip) % n
        skip += 1

    return lst[0] * lst[1]

 


def part_two(lengths: list[int]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lengths: list[int] = []
    for line in fileinput.input():
        line = line.strip()
        lengths = list(map(int, line.split(',')))
        break
        


    # too high: 30012
    print('part_one', part_one(list(range(256)), lengths))

    print('part_two', part_two(lengths))


if __name__ == '__main__':
    main()
    
def test_part_one():
    """Test part one."""
    assert part_one([0,1,2,3,4],[3, 4, 1, 5]) == 12







