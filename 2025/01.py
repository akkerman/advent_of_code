"""2025 Day 1: Secret Entrance"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def part_one(instructions: list[tuple[str, int]]):
    """Solution to part one."""
    pos = 50
    zeroes = 0
    for d, n in instructions:
        if d == 'L':
            pos = (pos - n) % 100
        elif d == 'R':
            pos = (pos + n) % 100

        if pos == 0:
            zeroes += 1

    return zeroes



def part_two(lines):
    """Solution to part two."""
    return 'todo'


re_dir = re.compile(r'([LR])(\d+)')
def main():
    """Parse input file, pass to puzzle solvers."""
    instructions = []
    for line in fileinput.input():
        line = line.strip()
        if m := re_dir.match(line):
            d, n = m.groups()
            instructions.append((d, int(n)))

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()
