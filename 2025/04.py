"""2025 Day 4: Printing Department"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
Grid = list[Coord]

def neighbors(c:Coord):
    """Return all 8 neighboring coordinates."""
    x,y = c
    for dx in [-1,0,1]:
        for dy in [-1,0,1]:
            if dx == 0 and dy == 0:
                continue
            yield (x+dx, y+dy)

def mark_movable(grid:Grid):
    movable = set[Coord]()
    for x,y in grid:
        if sum(1 for c in neighbors((x,y)) if c in grid) < 4:
            movable.add((x,y))
    return movable

def part_one(grid:Grid):
    """Solution to part one."""
    return len(mark_movable(grid))
            


def part_two(grid:Grid):
    """Solution to part two."""
    current = set[Coord](grid)

    while True:
        movable = mark_movable(current)
        if not movable:
            return len(grid) - len(current)

        current = current - movable



def main():
    """Parse input file, pass to puzzle solvers."""
    grid = Grid()

    for y,line in enumerate(fileinput.input()):
        for x,c in enumerate(line.strip()):
            if c == '@':
                grid.append((x,y))


    print('part_one', part_one(grid))

    print('part_two', part_two(grid))


if __name__ == '__main__':
    main()
