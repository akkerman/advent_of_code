"""2017 Day 17: Spinlock"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def part_one(steps: int):
    """Solution to part one."""
    buffer = [0]
    pos = 0
    for i in range(1, 2017 + 1):
        pos = (pos + steps) % len(buffer) + 1
        buffer.insert(pos, i)
    return buffer[(pos + 1) % len(buffer)]


def part_two(steps: int):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse steps file, pass to puzzle solvers."""
    steps: int = int(next(fileinput.input()).strip())

    print('part_one', part_one(steps))

    print('part_two', part_two(steps))


if __name__ == '__main__':
    main()
