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

def point_polygon(test: Coord, tiles: list[Coord]) -> int:
    """Ray-casting algorithm to determine if point is in polygon."""
    px,py = test
    intersections = sum(1
               for (x1,y1), (x2,y2) in zip(tiles, tiles[1:]+[tiles[0]])
               if px < x1  and x1 == x2 and (y1 > py) != (y2 > py)
               # px < x1 + (py - y1) * (x2 - x1) / (y2 - y1) # snijpunt test niet nodig bij orthogonale edges
               )
    return intersections % 2 == 1
 
def point_on_edge(test: Coord, tiles: list[Coord]) -> bool:
    """Check if point is on any polygon edge."""
    px,py = test
    for (x1,y1), (x2,y2) in zip(tiles, tiles[1:]+[tiles[0]]):
        if x1 == x2 == px and min(y1,y2) <= py <= max(y1,y2):
            return True
        if y1 == y2 == py and min(x1,x2) <= px <= max(x1,x2):
            return True
    return False

def is_parallel(edge1: tuple[Coord, Coord], edge2: tuple[Coord, Coord]) -> bool:
    """Check if two edges are parallel."""
    (x1,y1), (x2,y2) = edge1
    (x3,y3), (x4,y4) = edge2
    return (x1 == x2 and x3 == x4) or (y1 == y2 and y3 == y4)

def do_intersect(edge1: tuple[Coord, Coord], edge2: tuple[Coord, Coord]) -> bool:
    """Check if two edges intersect."""
    (px1,py1), (qx1,qy1) = edge1
    (px2,py2), (qx2,qy2) = edge2
    if is_parallel(edge1, edge2):
        return False
    if px1 == qx1:  # edge1 is vertical
        return (min(py2,qy2) <= py1 <= max(py2,qy2)) and (min(px1,qx1) <= px2 <= max(px1,qx1))
    else:  # edge1 is horizontal
        return (min(px2,qx2) <= px1 <= max(px2,qx2)) and (min(py1,qy1) <= py2 <= max(py1,qy1))

def polygon_is_axis_aligned(tiles: list[Coord]) -> bool:
    """Check if all polygon edges are axis-aligned."""
    return all(x1 == x2 or y1 == y2 
               for (x1,y1), (x2,y2) in zip(tiles, tiles[1:]+[tiles[0]]))


def main():
    """Parse input file, pass to puzzle solvers."""
    tiles = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert(len(coord) == 2)
        tiles.append(coord)



    # all polygon edges are axis-aligned
    assert polygon_is_axis_aligned(tiles)


    print(point_polygon((10,5), [(20,0),(20, 5), (30, 5), (30,10), (5,10), (5,0)]))

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
