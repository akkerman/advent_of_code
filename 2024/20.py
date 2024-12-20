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

def annotate(track: set[Coord], start: Coord, end: Coord) -> dict[Coord, int]:
    annotated: dict[Coord, int] = {}
    q = deque([(0, start)])
    while q:
        length, coord = q.popleft()
        if coord in annotated: continue
        annotated[coord] = length

        for dr, dc in ((0,1), (1,0), (0,-1), (-1,0)):
            next = (coord[0]+dr, coord[1]+dc)
            if next not in track: continue
            q.append((length+1, next))

    annotated[start] = 0
    return annotated


@perf_timer
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

    # for saves, cheats in sorted(counter.items()):
    #     print(f'There are {cheats} cheats that save {saves} picoseconds.')

    return sum(cheat for save,cheat in counter.items() if save >= 100)

@perf_timer
def part_one_v2(track: set[Coord], wall:set[Coord], start: Coord, end: Coord, bounds: Coord) -> int:
    """Count the number of cheats that save at least 100 picoseconds."""
    def is_outer(coord: Coord) -> bool:
        r,c = coord
        br, bc = bounds
        return r == 0 or c == 0 or r == br or c == bc

    max_path = len(track) -1 # don't count the start
    candidates = { c for c in wall if not is_outer(c) }

    annotated = annotate(track, start, end)
    counter: Counter[int] = Counter()

    for tr in track:
        if tr not in annotated:
            print('no path to', tr)

    dirs = [(0,1), (1,0), (0,-1), (-1,0)]
    for r,c in candidates:
        neighbors = [annotated[(r+dr, c+dc)] for dr,dc in dirs if (r+dr, c+dc) in track]
        if len(neighbors) < 2: continue
        neighbors.sort()
        save = neighbors[-1] - neighbors[0] - 2
        if save == 1:
            print(r,c, neighbors)
        counter.update({save: 1})


    # for saves, cheats in sorted(counter.items()):
    #     print(f'There are {cheats} cheats that save {saves} picoseconds.')

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

    print('part_one_v2', part_one_v2(track, wall, start, end, (row, col)))
    # print('part_one', part_one(track, wall, start, end, (row, col)))
    

    print('part_two', part_two([]))


if __name__ == '__main__':
    main()
