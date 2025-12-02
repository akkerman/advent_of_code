"""2025 Day 2: Gift Shop"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def is_invalid_id(id: int) -> bool:
    """Check if the given ID string is invalid based on some criteria."""
    id_str = str(id)
    mid = len(id_str) // 2
    return id_str[:mid] == id_str[mid:]

def part_one(ranges:list[tuple[int,int]]):
    """Solution to part one."""
    total = 0
    for x,y in ranges:
        for id in range(x, y+1):
            if is_invalid_id(id):
                total += id
    return total


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    line: str = next(fileinput.input()).strip()
    ranges: list[tuple[int,int]] = [(int(x),int(y)) for x,y in re.findall(r'(\d+)-(\d+)', line)]

    print('part_one', part_one(ranges))

    print('part_two', part_two(ranges))


if __name__ == '__main__':
    main()
