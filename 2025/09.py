"""2025 Day 9: Movie Theater"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

from itertools import combinations

Coord = tuple[int, int]

def area(a: Coord, b: Coord) -> int:
    """Calculate area of rectangle defined by two coords."""
    width = abs(a[0] - b[0]) + 1
    height = abs(a[1] - b[1]) + 1
    return width * height

def part_one(tiles: list[Coord]):
    """Solution to part one."""
    return max(area(a,b) for a,b in combinations(tiles, 2))


def part_two(tiles: list[Coord]):
    """Solution to part two."""

    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    tiles = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert(len(coord) == 2)
        tiles.append(coord)


    print('part_one', part_one(tiles))

    print('part_two', part_two(tiles))


if __name__ == '__main__':
    main()

def test_area1():
    assert area((2,5),(9,7)) == 24
def test_area2():
    assert area((7,1),(11,7)) == 35
def test_area3():
    assert area((2,5), (11,1)) == 50
