"""Day 20: Race Condition."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int] # row, col
Direction = tuple[int, int] # row, col

def race(track: set[Coord], start: Coord, end: Coord) -> int:
    q = [(0, start)]
    visited: set[Coord] = set()
    while q:
        length, coord = heapq.heappop(q)
        if coord == end: return length
        if coord in visited: continue
        visited.add(coord)

        for dr, dc in ((0,1), (1,0), (0,-1), (-1,0)):
            next = (coord[0]+dr, coord[1]+dc)
            if next not in track: continue
            heapq.heappush(q, (length+1, next))
    return -1 # no path


def part_one(track: set[Coord], wall:set[Coord], start: Coord, end: Coord, bounds: Coord) -> int:
    """Count the number of cheats that save at least 100 picoseconds."""
    def is_outer(coord: Coord) -> bool:
        r,c = coord
        br, bc = bounds
        return r == 0 or c == 0 or r == br or c == bc

    max_path = len(track) -1 # don't count the start
    candidates = { c for c in wall if not is_outer(c) }

    
    counter: Counter[int] = Counter()
    for c in candidates:
        t = track.copy()
        t.add(c)
        length = race(t, start, end)
        save = max_path - length
        counter.update({save: 1})

    for saves, cheats in sorted(counter.items()):
        print(f'There are {cheats} cheats that save {saves} picoseconds.')

    return sum(cheat for save,cheat in counter.items() if save >= 100)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    track: set[Coord] = set()
    wall: set[Coord] = set()
    row = 0
    col = 0
    start = (-1, -1)
    end = (-1, -1)
    for line in fileinput.input():
        line = line.strip()
        track.update((row,c) for c,d in enumerate(line) if d != '#')
        wall.update((row,c) for c,d in enumerate(line) if d == '#')
        if 'S' in line:
            start = (row, line.index('S'))
        if 'E' in line:
            end = (row, line.index('E'))

        row += 1
        col = len(line)

    print('part_one', part_one(track, wall, start, end, (row, col)))

    print('part_two', part_two([]))


if __name__ == '__main__':
    main()
