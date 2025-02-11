"""Day 20: Donut Maze."""
import fileinput
import heapq
from collections import deque, defaultdict
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


def portal_distances(path:set[Coord], coords:set[Coord], portals:dict[Coord, Coord]) -> dict[Coord, dict[Coord, int]]:
    """Calculate distances between all portals."""
    distances = defaultdict[Coord, dict[Coord, int]](dict)

    def next_steps(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        for nr, nc in ((r, c+1), (r, c-1), (r+1, c), (r-1, c)):
            if (nr, nc) in path:
                ns.add((nr, nc))
        return ns

    for start in coords:
        for end in coords:
            if start == end:
                continue
            if start in portals:
                distances[start][portals[start]] = 1

            q = deque[tuple[int, Coord]]([(0, start)])
            visited = set[Coord]()
            while q:
                steps, coord = q.popleft()
                if coord == end:
                    distances[start][end] = steps
                    distances[end][start] = steps
                    break

                if coord in visited:
                    continue
                visited.add(coord)

                for nxt in next_steps(coord):
                    if nxt in visited:
                        continue
                    q.append((steps+1, nxt))
    return distances


@perf_timer
def bfs_part_one(path: set[Coord], portals: dict[Coord, Coord], start: Coord, end: Coord) -> int:
    def next_steps(coord: Coord):
        ns = set[Coord]()
        r,c = coord
        for nr, nc in ((r, c+1), (r, c-1), (r+1, c), (r-1, c)):
            if (nr, nc) in path:
                ns.add((nr, nc))

        if coord in portals:
            ns.add(portals[coord])

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



@perf_timer
def part_one(lines:list[str]):
    """Fewest steps through the maze."""
    path, labels, portals, _ = parse_input(lines)
    start = labels['AA'][0]
    end = labels['ZZ'][0]
    return bfs_part_one(path, portals, start, end)


@perf_timer
def bfs_part_one_v2(distances:dict[Coord, dict[Coord, int]], start: Coord, end: Coord) -> int:

    q = list[tuple[int, Coord]]([(0, start)])
    visited = set[Coord]()
    while q:
        steps, coord = heapq.heappop(q)
        if coord == end:
            return steps

        if coord in visited:
            continue
        visited.add(coord)

        for nxt_coord, nxt_steps in distances[coord].items():
            if nxt_coord in visited:
                continue
            heapq.heappush(q, (steps+nxt_steps, nxt_coord))

    return -1

@perf_timer
def part_one_v2(lines:list[str]):
    """Fewest steps through the maze."""
    path, labels, portals, rev_portals = parse_input(lines)
    portal_coords = rev_portals.keys()
    distances = portal_distances(path, set(portal_coords), portals)
    start = labels['AA'][0]
    end = labels['ZZ'][0]
    return bfs_part_one_v2(distances, start, end)

def print_path(locs:list[tuple[int, int, Coord]], rev_portals:dict[Coord, str]):
    for loc in locs:
        print(rev_portals[loc[2]],loc)

    print()
    print('steps through maze')
    prev_steps = 0
    prev_level = 0
    for i, tpl in enumerate(locs[1:]):
        steps, level, loc = tpl

        intermediate = steps - prev_steps
        prev_steps = steps

        lbl = rev_portals[loc]
        prev_coord = locs[i][2]
        prev_lbl = rev_portals[prev_coord]

        if intermediate == 1:
            if prev_level < level:
                print(f'Recurse into level {level} through {lbl} ({intermediate} step)')
            else:
                print(f'Return to level {level} through {lbl} ({intermediate} step)')
            prev_level = level
        else:
            print(f'Walk from {prev_lbl} to {lbl} ({intermediate} steps)')


@perf_timer
def bfs_part_two(distances:dict[Coord, dict[Coord, int]], start: Coord, end: Coord, rev_portals:dict[Coord, str]) -> int:

    min_row = min(r for r, _ in distances.keys())
    max_row = max(r for r, _ in distances.keys())
    min_col = min(c for _, c in distances.keys())
    max_col = max(c for _, c in distances.keys())

    def is_outer(coord:Coord) -> bool:
        r,c = coord
        return r in (min_row, max_row) or c in (min_col, max_col)

    q = list[tuple[int, int, Coord, list[tuple[int, int, Coord]]]]([(0, 0, start, [(0, 0, start)])])
    visited = set[tuple[int,Coord]]()
    while q:
        steps, level, coord, locs = heapq.heappop(q)
        if steps > 15000:
            raise ValueError('Too many steps')
        if coord == end and level == 0:
            # print_path(locs, rev_portals)
            return steps

        if (level, coord) in visited:
            continue
        visited.add((level, coord))

        for nxt_coord, nxt_steps in distances[coord].items():
            nxt_level = level
            if nxt_steps == 1: # portal jump
                if is_outer(coord):
                    nxt_level -= 1
                else:
                    nxt_level += 1

            if nxt_level < 0:
                continue

            if (nxt_level, nxt_coord) in visited:
                continue
            
            tmp_steps = steps + nxt_steps
            heapq.heappush(q, (tmp_steps, nxt_level, nxt_coord, locs + [(tmp_steps, nxt_level, nxt_coord)]))

    return -1



@perf_timer
def part_two(lines:list[str]):
    """Fewest steps through the recursive maze."""
    path, labels, portals, rev_portals = parse_input(lines)
    portal_coords = rev_portals.keys()
    distances = portal_distances(path, set(portal_coords), portals)
    start = labels['AA'][0]
    end = labels['ZZ'][0]
    return bfs_part_two(distances, start, end, rev_portals)

def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        lines.append(line.replace('\n', ''))

    print('part_one', part_one(lines))
    print('part_one_v2', part_one_v2(lines))
    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
    print('done')

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
