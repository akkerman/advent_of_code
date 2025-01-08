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
            raise ValueError('Invalid input')

        l1 = (r,c)
        l2 = part.pop()
        labels[label(l1, l2)].append(coords.pop())

    rev_labels = dict[Coord, str]()
    for lbl, coords in labels.items():
        if len(coords) == 2:
            portals[coords[0]] = coords[1]
            portals[coords[1]] = coords[0]
            rev_labels[coords[0]] = lbl
            rev_labels[coords[1]] = lbl
        rev_labels[coords[0]] = lbl

    return path, labels, portals, rev_labels


def part_one(lines:list[str]):
    """Fewest steps through the maze."""
    path, labels, portals, _ = parse_input(lines)
    start = labels['AA'][0]
    end = labels['ZZ'][0]

    def next_steps(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        for nr, nc in ((r, c+1), (r, c-1), (r+1, c), (r-1, c)):
            if (nr, nc) in path:
                ns.add((nr, nc))
        return ns

    def next_portal(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        if (r, c) in portals:
            ns.add(portals[(r, c)])
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

        for nxt in next_portal(coord):
            if nxt in visited:
                continue
            q.append((steps+1, nxt))

    return -1


def part_two(lines:list[str]):
    """Fewest steps through the recursive maze."""
    path, labels, portals, rev_labels = parse_input(lines)
    start = labels['AA'][0]
    end = labels['ZZ'][0]
    
    min_row = min(r for r, _ in portals.keys())
    max_row = max(r for r, _ in portals.keys())
    min_col = min(c for _, c in portals.keys())
    max_col = max(c for _, c in portals.keys())

    def is_outer(coord:Coord) -> bool:
        r,c = coord
        return r in (min_row, max_row) or c in (min_col, max_col)

    def next_steps(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        for nr, nc in ((r, c+1), (r, c-1), (r+1, c), (r-1, c)):
            if (nr, nc) in path:
                ns.add((nr, nc))
        return ns

    def next_portal(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        if (r, c) in portals:
            ns.add(portals[(r, c)])
        return ns

    q = list[tuple[int, int, Coord]]([(0, 0, start)])
    visited = set[tuple[int,Coord]]()
    while q:
        steps, level, coord = heapq.heappop(q)
        if coord == end and level == 0:
            return steps

        if (level, coord) in visited:
            continue
        visited.add((level, coord))

        for nxt in next_steps(coord):
            if (level,nxt) in visited:
                continue
            heapq.heappush(q, (steps+1, level, nxt))

        for nxt in next_portal(coord):
            if (level, nxt) in visited:
                continue
            if is_outer(nxt):
                level -= 1
            else:
                level += 1

            if level < 0:
                continue

            # print(f'portal {rev_labels[coord]}: {coord} -> {nxt} level: {level}')

            # print(steps, level, nxt)
            heapq.heappush(q, (steps+1, level, nxt))

    return -1


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        lines.append(line)

    print('part_one', part_one(lines))

    # print('part_two', part_two(lines))


if __name__ == '__main__':
    main()

class Test_part_one:
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

    def test_example_2(self):
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

class Test_part_two:
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
        assert part_two(lines) == 26

    def test_example_interesting(self):
        lines = [
            '             Z L X W       C                 ',
            '             Z P Q B       K                 ',
            '  ###########.#.#.#.#######.###############  ',
            '  #...#.......#.#.......#.#.......#.#.#...#  ',
            '  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  ',
            '  #.#...#.#.#...#.#.#...#...#...#.#.......#  ',
            '  #.###.#######.###.###.#.###.###.#.#######  ',
            '  #...#.......#.#...#...#.............#...#  ',
            '  #.#########.#######.#.#######.#######.###  ',
            '  #...#.#    F       R I       Z    #.#.#.#  ',
            '  #.###.#    D       E C       H    #.#.#.#  ',
            '  #.#...#                           #...#.#  ',
            '  #.###.#                           #.###.#  ',
            '  #.#....OA                       WB..#.#..ZH',
            '  #.###.#                           #.#.#.#  ',
            'CJ......#                           #.....#  ',
            '  #######                           #######  ',
            '  #.#....CK                         #......IC',
            '  #.###.#                           #.###.#  ',
            '  #.....#                           #...#.#  ',
            '  ###.###                           #.#.#.#  ',
            'XF....#.#                         RF..#.#.#  ',
            '  #####.#                           #######  ',
            '  #......CJ                       NM..#...#  ',
            '  ###.#.#                           #.###.#  ',
            'RE....#.#                           #......RF',
            '  ###.###        X   X       L      #.#.#.#  ',
            '  #.....#        F   Q       P      #.#.#.#  ',
            '  ###.###########.###.#######.#########.###  ',
            '  #.....#...#.....#.......#...#.....#.#...#  ',
            '  #####.#.###.#######.#######.###.###.#.#.#  ',
            '  #.......#.......#.#.#.#.#...#...#...#.#.#  ',
            '  #####.###.#####.#.#.#.#.###.###.#.###.###  ',
            '  #.......#.....#.#...#...............#...#  ',
            '  #############.#.#.###.###################  ',
            '               A O F   N                     ',
            '               A A D   M                     ',
        ]
        assert part_two(lines) == 396
