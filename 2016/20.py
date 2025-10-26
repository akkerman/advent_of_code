"""Day 20: Firewall Rules."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer


Interval = tuple[int, int]

def part_one(intervals: list[Interval]):
    """Solution to part one."""

    from_ip = 0
    to_ip = 0
    for f, t in sorted(intervals, key=lambda x: x[0]):
        if from_ip <= f <= to_ip:
            to_ip = max(to_ip, t)
            continue
        elif f == to_ip + 1:
            to_ip = max(to_ip, t)
            continue
        else:
            assert to_ip < f
            return to_ip + 1


    return 'todo'


def part_two(intervals):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    intervals: list[Interval] = []
    for line in fileinput.input():
        line = line.strip()
        interval = tuple(map(int, line.split('-')))
        assert len(interval) == 2
        
        intervals.append(interval)

    # too low: 1847081
    print('part_one', part_one(intervals))

    print('part_two', part_two(intervals))


if __name__ == '__main__':
    main()
