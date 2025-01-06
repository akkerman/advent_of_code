"""Day 18: Many-Worlds Interpretation."""
import fileinput
import heapq
from utils import perf_timer

Coord = tuple[int, int]

def parse_input(lines:list[str]):
    """Parse input into a data structure."""
    walls = set[Coord]()
    keys = dict[Coord, str]()
    doors = dict[Coord, str]()
    entries: Coord = (-1, -1)
    for row, line in enumerate(lines):
        walls.update((row, col) for col, char in enumerate(line) if char == '#')
        keys.update({(row, col):char for col, char in enumerate(line) if char.islower()})
        doors.update({(row, col):char for col, char in enumerate(line) if char.isupper()})
        if '@' in line:
            entries = (row, line.index('@'))

    return walls, keys, doors, entries

def parse_input2(lines:list[str]):
    walls, keys, doors, start = parse_input(lines)
    r, c = start
    entries = [(r-1, c-1), (r-1, c+1), (r+1, c+1), (r+1, c-1)]
    walls.update([(r,c), (r-1, c), (r+1, c), (r, c+1), (r, c-1)])
    return walls, keys, doors, entries


@perf_timer
def part_one(lines:list[str]) -> int:
    """Length of shortest path to get all keys from one location."""
    walls, keys, doors, start = parse_input(lines)

    def next_steps(coord: Coord): 
        r,c = coord
        return (nxt for nxt in ((r, c+1), (r,c-1), (r+1, c), (r-1, c )) if nxt not in walls)


    all_keys = frozenset(keys.values())
    visited = set[tuple[Coord, frozenset[str]]]()

    q: list[tuple[int, Coord, frozenset[str]]] = [(0, start, frozenset())]

    while q:
        steps, coord, collected_keys = heapq.heappop(q)
        if collected_keys == all_keys:
            return steps

        if (coord, collected_keys) in visited:
            continue
        visited.add((coord, collected_keys))

        for nxt in next_steps(coord):
            if (nxt, collected_keys) in visited:
                continue

            if (nxt in doors and doors[nxt].lower() not in collected_keys):
                visited.add((nxt, collected_keys))
                continue

            if nxt in keys and keys[nxt] not in collected_keys:
                heapq.heappush(q, (steps+1, nxt, collected_keys | {keys[nxt]}))
                continue

            heapq.heappush(q, (steps+1, nxt, collected_keys))

    return -1

@perf_timer
def part_two(lines:list[str]) -> int:
    """Length of shortest path to get all keys from 4 locations."""
    walls, keys, _, entries = parse_input2(lines)

    def next_steps(coord: Coord): 
        r,c = coord
        return (nxt for nxt in ((r, c+1), (r,c-1), (r+1, c), (r-1, c )) if nxt not in walls)

    def steps2keys(start: Coord, all_keys: frozenset[str]) -> int:
        visited = set[tuple[Coord, frozenset[str]]]()

        q: list[tuple[int, Coord, frozenset[str]]] = [(0, start, frozenset())]

        while q:
            steps, coord, collected_keys = heapq.heappop(q)
            if collected_keys == all_keys:
                return steps

            if (coord, collected_keys) in visited:
                continue
            visited.add((coord, collected_keys))

            for nxt in next_steps(coord):
                if (nxt, collected_keys) in visited:
                    continue

                if nxt in keys and keys[nxt] not in collected_keys:
                    heapq.heappush(q, (steps+1, nxt, collected_keys | {keys[nxt]}))
                    continue

                heapq.heappush(q, (steps+1, nxt, collected_keys))

        raise ValueError('No path found')

    total = 0

    r,c = entries[0] # top left

    robot_keys = frozenset([kv for (kr,kc),kv in keys.items() if kr<=r and kc<=c])
    total += steps2keys((r,c), robot_keys)
    
    r,c = entries[1] # top right
    robot_keys = frozenset([kv for (kr,kc),kv in keys.items() if kr<=r and kc>=c])
    total += steps2keys((r,c), robot_keys)

    r,c = entries[2] # bottom right
    robot_keys = frozenset([kv for (kr,kc),kv in keys.items() if kr>=r and kc>=c])
    total += steps2keys((r,c), robot_keys)

    r,c = entries[3] # bottom left
    robot_keys = frozenset([kv for (kr,kc),kv in keys.items() if kr>=r and kc<=c])
    total += steps2keys((r,c), robot_keys)
    
    return total


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        line = line.strip()
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()


class Test_PartOne:
    def test_example_1(self):
        lines = [
            '#########',
            '#b.A.@.a#',
            '#########',
        ]
        assert part_one(lines) == 8

    def test_example_2(self):
        lines = [
            '########################',
            '#f.D.E.e.C.b.A.@.a.B.c.#',
            '######################.#',
            '#d.....................#',
            '########################',
        ]
        assert part_one(lines) == 86

    def test_example_3(self):
        lines = [
            '########################',
            '#...............b.C.D.f#',
            '#.######################',
            '#.....@.a.B.c.d.A.e.F.g#',
            '########################',
        ]
        assert part_one(lines) == 132

    def test_example_4(self):
        lines = [
            '#################',
            '#i.G..c...e..H.p#',
            '########.########',
            '#j.A..b...f..D.o#',
            '########@########',
            '#k.E..a...g..B.n#',
            '########.########',
            '#l.F..d...h..C.m#',
            '#################',
        ]
        assert part_one(lines) == 136

    def test_example_5(self):
        lines = [
            '########################',
            '#@..............ac.GI.b#',
            '###d#e#f################',
            '###A#B#C################',
            '###g#h#i################',
            '########################',
        ]
        assert part_one(lines) == 81

class Test_PartTwo:
    def test_example_1(self):
        lines = [
            '#######',
            '#a.#Cd#',
            '##...##',
            '##.@.##',
            '##...##',
            '#cB#Ab#',
            '#######',
        ]
        assert part_two(lines) == 8

    def test_example_2(self):
        lines = [
            '###############',
            '#d.ABC.#.....a#',
            '######...######',
            '######.@.######',
            '######...######',
            '#b.....#.....c#',
            '###############',
        ]
        assert part_two(lines) == 24

    def xtest_example_3(self):
        lines = [
            '#############',
            '#DcBa.#.GhKl#',
            '#.###...#I###',
            '#e#d#.@.#j#k#',
            '###C#...###J#',
            '#fEbA.#.FgHi#',
            '#############',
        ]
        assert part_two(lines) == 32

    def xtest_example_4(self):
        lines = [
            '#############',
            '#g#f.D#..h#l#',
            '#F###e#E###.#',
            '#dCba...BcIJ#',
            '#####.@.#####',
            '#nK.L...G...#',
            '#M###N#H###.#',
            '#o#m..#i#jk.#',
            '#############',
        ]
        assert part_two(lines) == 72
