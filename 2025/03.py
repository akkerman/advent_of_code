"""2025 Day 3: Lobby"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Battery = list[int]


def max_joltage(battery: Battery) -> int:
    """Return the maximum joltage of the battery."""
    tiental = max(battery[:-1])
    idx = battery.index(tiental)
    eenheid = max(battery[idx + 1 :])
    return 10 * tiental  + eenheid
def part_one(batteries: list[Battery]):
    """Solution to part one."""
    return sum(max_joltage(battery) for battery in batteries)


def part_two(batteries):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    batteries: list[Battery] = []
    for line in fileinput.input():
        batery = [int(x) for x in line.strip()]
        batteries.append(batery)

    print('part_one', part_one(batteries))

    print('part_two', part_two(batteries))


if __name__ == '__main__':
    main()
