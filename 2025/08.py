"""2025 Day 8: Playground"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

import math

Coord = tuple[int, int, int]

def euclidean_distance(a: Coord, b: Coord) -> float:
    """Calculate Euclidean distance between two 3D coordinates."""
    return ((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2) ** 0.5

def part_one(locations: list[Coord]):
    """Solution to part one."""
    distances = dict[tuple[int, int], float]()
    for i, coord in enumerate(locations):
        for j in range(i + 1, len(locations)):
            other = locations[j]
            idx = (i, j) if i < j else (j, i)
            dist = euclidean_distance(coord, other)
            distances[idx] = dist

    sorted_distances = sorted(distances.items(), key=lambda x: x[1])

    connected = dict[int, set[int]]()
    max = 10 if len(locations) < 1000 else 1000

    for idx in range(max):
        sd = sorted_distances[idx]
        (i, j), dist = sd

        if i not in connected and j not in connected:
            pair = set[int]([i,j])
            connected[i] = pair
            connected[j] = pair
            continue

        if i in connected and j in connected:
            # both are connect to other nodes
            if connected[i] is connected[j]:
                continue
            else: # merge sets
                assert i not in connected[j] and j not in connected[i]

                union = connected[i] | connected[j]

                for member in union:
                    connected[member] = union

                continue
        if j in connected:
            connected[j].add(i)
            connected[i] = connected[j]
            continue
        if i in connected:
            connected[i].add(j)
            connected[j] = connected[i]
            continue

        assert False, "Should not reach here"
     
    
    # sorted values by length
    uniq = {tuple(conn) for conn in connected.values()}
    

    groups = sorted(uniq, key=lambda x: len(x), reverse=True)
    first_three = groups[:3]
    return math.prod(len(g) for g in first_three)



def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    locations = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert len(coord) == 3
        locations.append(coord)


   # too low 576
    print('part_one', part_one(locations))

    print('part_two', part_two(locations))


if __name__ == '__main__':
    main()
