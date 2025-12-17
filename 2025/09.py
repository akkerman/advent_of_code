"""2025 Day 9: Movie Theater"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

from itertools import combinations
import subprocess

Coord = tuple[int, int]
Edge = tuple[Coord, Coord]

def area(a: Coord, b: Coord) -> int:
    """Calculate area of rectangle defined by two coords."""
    width = abs(a[0] - b[0]) + 1
    height = abs(a[1] - b[1]) + 1
    return width * height

def part_one(tiles: list[Coord]):
    """Solution to part one."""
    return max(area(a,b) for a,b in combinations(tiles, 2))

def sorted_edges(tiles: list[Coord]) -> list[tuple[int, Edge]]:
    """Return list of polygon edges sorted distant to eachother."""
    edges:list[tuple[int,Edge]] = []

    for edge in zip(tiles, tiles[1:]+[tiles[0]]):
        (x1,y1), (x2,y2) = edge
        distance = abs(x1 - x2) + abs(y1 - y2)
        edges.append((distance, edge))

    return sorted(edges, key=lambda e: -e[0])


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
        assert(py2 == qy2)
        return min(py1,qy1) < py2 < max(py1,qy1) and min(px2, qx2) < px1 < max(px2,qx2)
    else:  # edge1 is horizontal
        assert(px2 == qx2)
        return min(py2,qy2) < py1 < max(py2,qy2) and min(px1,qx1) < px2 < max(px1, qx1)




def part_two(tiles: list[Coord]):
    """Solution to part two."""
    def rect_in_polygon(a: Coord, b: Coord) -> bool:
        """Determine if rectangle defined by two coords is fully inside polygon."""
        corners = [a, (a[0], b[1]), b, (b[0], a[1])]
        if any(not point_polygon(corner, tiles) for corner in corners):
            return False

        rectangle_edges = list(zip(corners, corners[1:]+[corners[0]]))
        for polygon_edge in zip(tiles, tiles[1:]+[tiles[0]]):
            for rectangle_edge in rectangle_edges:
                if do_intersect(rectangle_edge, polygon_edge):
                    return False

        return True

    def area_if_inside(a: Coord, b: Coord) -> int:
        """Calculate area of rectangle defined by two coords if fully inside polygon."""
        return area(a,b) if rect_in_polygon(a,b) else 0

    return max(area_if_inside(a,b) for a,b in combinations(tiles, 2))

def polygon_is_axis_aligned(tiles: list[Coord]) -> bool:
    """Check if all polygon edges are axis-aligned."""
    return all(x1 == x2 or y1 == y2 
               for (x1,y1), (x2,y2) in zip(tiles, tiles[1:]+[tiles[0]]))

def plot_polygon(tiles: list[tuple[int, int]]) -> None:
    scale = 100
    with open('09-polygon.dot', 'w') as f:
        f.write('graph polygon {\n')
        f.write('   layout = neato;\n')
        f.write('   node [shape=point];\n')

        for idx, (x, y) in enumerate(tiles):
            f.write(f'     p{idx} [pos="{x/scale},{y/scale}!"];\n')
            
        idxs = list(range(len(tiles))) + [0]
        for i in range(len(tiles)):
            f.write(f'     p{idxs[i]} -- p{idxs[i+1]};\n')

        f.write('}\n')

    subprocess.run(
    [
        "neato",
        "-n",
        "-s0.1",
        "-Tpng",
        "09-polygon.dot",
        "-o",
        "09-polygon.png",
    ],
    check=True,
    )


def main():
    """Parse input file, pass to puzzle solvers."""
    tiles = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert(len(coord) == 2)
        tiles.append(coord)

    # plot_polygon(tiles)

    # all polygon edges are axis-aligned
    # assert polygon_is_axis_aligned(tiles)

    # for entry in sorted_edges(tiles)[:5]:
    #     print(entry)

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
