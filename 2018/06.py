"""Day 6: Chronal Coordinates."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]

def manhattan(a: Coord, b: Coord) -> int:
    """Calculate the Manhattan distance between two coordinates."""
    return abs(a[0] - b[0]) + abs(a[1] - b[1])

def part_one(coords: list[Coord] ):
    """Solution to part one."""
    min_x = min(x for x,_ in coords)
    max_x = max(x for x,_ in coords)
    min_y = min(y for _,y in coords)
    max_y = max(y for _,y in coords)
    
    inf_areas: set[Coord] = set()
    area_count: Counter[Coord] = Counter()

    for x in range(min_x, max_x + 1):
        for y in range(min_y, max_y + 1):
            dists = [manhattan((x,y), (ox,oy)) for ox,oy in coords]
            min_dist = min(dists)
            if dists.count(min_dist) == 1:
                closest = coords[dists.index(min_dist)]
                area_count[closest] += 1
                if x in (min_x, max_x) or y in (min_y, max_y):
                    inf_areas.add(closest)
        
    return max(size for coord, size in area_count.items() if coord not in inf_areas)


def part_two(coords: list[Coord]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    coords: list[Coord] = []
    for line in fileinput.input():
        line = line.strip()
        x,y = list(map(int, line.split(', ')))
        coords.append((x,y))

    print('part_one', part_one(coords))

    print('part_two', part_two(coords))


if __name__ == '__main__':
    main()
