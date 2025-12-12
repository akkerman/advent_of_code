"""2025 Day 12: Christmas Tree Farm"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
BoundingBox = tuple[int, int]
Requirements = tuple[int, int, int, int, int, int]
Region = tuple[BoundingBox, Requirements]

def part_one(lines):
    """Solution to part one."""
    return 'todo'


def part_two(lines):
    """Solution to part two."""
    return 'todo'

re_region = re.compile(r'(.+)x(.+): (.+) (.+) (.+) (.+) (.+) (.+)')

def main():
    """Parse input file, pass to puzzle solvers."""
    lines = []

    present_id = 0
    presents: dict[int, list[Coord]] = {}
    y: int = 0
    regions: list[Region] = []

    for line in fileinput.input():
        if 'x' in line:
            m = re_region.match(line)
            assert m is not None
            ints = [int(i) for i in m.groups()]
            region: Region = (
                    tuple(ints[:2]),
                    tuple(ints[2:])
            )
            regions.append(region)
        else:

            if ':' in line:
                present_id = int(line.split(':')[0])
                presents[present_id] = []
                y = 0
                continue

            for x, c in enumerate(line.strip()):
                if c == '#':
                    presents[present_id].append((x, y))
            y += 1


          

            
    for present in presents:
        print(present, ':',  presents[present])

    for region in regions:
        print(region)   


    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
