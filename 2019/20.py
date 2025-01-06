"""Day 20: Donut Maze."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
def parse_input(lines: list[str]):
    path = set[Coord]()
    letters = dict[Coord, str]()
    portals = dict[Coord, Coord]()

    labels = defaultdict[str, list[Coord]](list)

    for row, line in enumerate(lines):
        path.update((row, col) for col, char in enumerate(line) if char == '.')
        letters.update({(row, col):char for col, char in enumerate(line) if char.isupper()})

    def label(c1:Coord, c2:Coord) -> str:
        l1 = letters[c1]
        l2 = letters[c2]
        if c1[0] <= c2[0] and c1[1] <= c2[1]:
            return f'{l1}{l2}'
        return f'{l2}{l1}'

    for (r, c) in letters.keys():
        nbrs = { (r, c+1), (r, c-1), (r+1, c), (r-1, c) }

        coords = nbrs.intersection(path)
        if len(coords) != 1:
            continue
        part = nbrs.intersection(letters.keys())
        if len(part) != 1:
            print((r,c), letters[(r,c)], part, coords)
            print(letters)
            raise ValueError('Invalid input')

        l1 = (r,c)
        l2 = part.pop()
        labels[label(l1, l2)].append(coords.pop())

    for coords in labels.values():
        if len(coords) == 2:
            portals[coords[0]] = coords[1]
            portals[coords[1]] = coords[0]

    return path, labels, portals


def part_one(lines:list[str]):
    """Solution to part one."""
    path, labels, portals = parse_input(lines)
    print(labels)
    start = labels['AA'][0]
    end = labels['ZZ'][0]

    def next_steps(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        for nr, nc in ((r, c+1), (r, c-1), (r+1, c), (r-1, c)):
            if (nr, nc) in path:
                ns.add((nr, nc))
            if (nr, nc) in portals:
                ns.add(portals[(nr, nc)])
        return ns

    q = deque[tuple[int, Coord]]([(0, start)])
    visited = set[Coord]()
    while q:
        steps, coord = q.popleft()
        if coord == end:
            return steps

        if coord in visited:
            continue
        visited.add(coord)

        for nxt in next_steps(coord):
            if nxt in visited:
                continue
            q.append((steps+1, nxt))

    return -1


def part_two(lines:list[str]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()

class Test_PartOne:
    def test_example_1(self):
        lines = [
            '         A         ',
            '         A         ',  
            '  #######.#########',  
            '  #######.........#',  
            '  #######.#######.#',  
            '  #######.#######.#',  
            '  #######.#######.#',  
            '  #####  B    ###.#',  
            'BC...##  C    ###.#',  
            '  ##.##       ###.#',  
            '  ##...DE  F  ###.#',  
            '  #####    G  ###.#',  
            '  #########.#####.#',  
            'DE..#######...###.#',  
            '  #.#########.###.#',  
            'FG..#########.....#',  
            '  ###########.#####',  
            '             Z     ',  
            '             Z     ',  
        ]
        assert part_one(lines) == 23

    def xtest_example_2(self):
        lines = [
           '                   A               ',
           '                   A               ',
           '  #################.#############  ',
           '  #.#...#...................#.#.#  ',
           '  #.#.#.###.###.###.#########.#.#  ',
           '  #.#.#.......#...#.....#.#.#...#  ',
           '  #.#########.###.#####.#.#.###.#  ',
           '  #.............#.#.....#.......#  ',
           '  ###.###########.###.#####.#.#.#  ',
           '  #.....#        A   C    #.#.#.#  ',
           '  #######        S   P    #####.#  ',
           '  #.#...#                 #......VT',
           '  #.#.#.#                 #.#####  ',
           '  #...#.#               YN....#.#  ',
           '  #.###.#                 #####.#  ',
           'DI....#.#                 #.....#  ',
           '  #####.#                 #.###.#  ',
           'ZZ......#               QG....#..AS',
           '  ###.###                 #######  ',
           'JO..#.#.#                 #.....#  ',
           '  #.#.#.#                 ###.#.#  ',
           '  #...#..DI             BU....#..LF',
           '  #####.#                 #.#####  ',
           'YN......#               VT..#....QG',
           '  #.###.#                 #.###.#  ',
           '  #.#...#                 #.....#  ',
           '  ###.###    J L     J    #.#.###  ',
           '  #.....#    O F     P    #.#...#  ',
           '  #.###.#####.#.#####.#####.###.#  ',
           '  #...#.#.#...#.....#.....#.#...#  ',
           '  #.#####.###.###.#.#.#########.#  ',
           '  #...#.#.....#...#.#.#.#.....#.#  ',
           '  #.###.#####.###.###.#.#.#######  ',
           '  #.#.........#...#.............#  ',
           '  #########.###.###.#############  ',
           '           B   J   C               ',
           '           U   P   P               ',
        ]
        assert part_one(lines) == 58


