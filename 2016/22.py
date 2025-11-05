"""Day 22: Grid Computing."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

from itertools import permutations

class DiskUsage():
    """Disk usage tuple."""
    def __init__(self, fs: str, size: int, used: int, avail: int, usep: int):
        self.fs = fs
        self.size = size
        self.used = used
        self.avail = avail
        self.usep = usep


def part_one(disk_usages: list[DiskUsage]):
    """Solution to part one."""
    return sum(
            1 for a, b in permutations(disk_usages, 2)
            if a.used != 0 and a != b and a.used <= b.avail
            )
    


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    disk_usages: list[DiskUsage] = []
    for line in fileinput.input():
        if not 'dev' in line:
            continue

        fs, size, used, avail, usep = line.strip().split()

        size = int(size[:-1])
        used = int(used[:-1])
        avail = int(avail[:-1])
        usep = int(usep[:-1])
        
        disk_usages.append(DiskUsage(fs, size, used, avail, usep))

    print('part_one', part_one(disk_usages))

    print('part_two', part_two(disk_usages))


if __name__ == '__main__':
    main()
