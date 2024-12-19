"""Day 9: Sensor Boost."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache


from utils import perf_timer

def part_one(program:list[int]):
    """Solution to part one."""
    print(program)
    return 'todo'


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    program: list[int] = []
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
