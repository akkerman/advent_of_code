"""Day 18: Many-Worlds Interpretation."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]

def parse_input(lines:list[str]):
    """Parse input into a data structure."""
    walls = set[Coord]()
    keys = dict[Coord, str]()
    doors = dict[Coord, str]()
    start: Coord = (-1, -1)
    for row, line in enumerate(lines):
        walls.update((row, col) for col, char in enumerate(line) if char == '#')
        keys.update({(row, col):char for col, char in enumerate(line) if char.islower()})
        doors.update({(row, col):char for col, char in enumerate(line) if char.isupper()})
        if '@' in line:
            start = (row, line.index('@'))

    return walls, keys, doors, start

def set2tuple(s:set[str]):
    return tuple(sorted(s))

@perf_timer
def part_one(lines:list[str]) -> int:
    """Solution to part one."""
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

def part_two(lines):
    """Solution to part two."""
    return 'todo'


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

def test_part_one_example_1():
    lines = [
        '#########',
        '#b.A.@.a#',
        '#########',
    ]
    assert part_one(lines) == 8

def test_part_one_example_2():
    lines = [
        '########################',
        '#f.D.E.e.C.b.A.@.a.B.c.#',
        '######################.#',
        '#d.....................#',
        '########################',
    ]
    assert part_one(lines) == 86

def test_part_one_example_3():
    lines = [
        '########################',
        '#...............b.C.D.f#',
        '#.######################',
        '#.....@.a.B.c.d.A.e.F.g#',
        '########################',
    ]
    assert part_one(lines) == 132

def test_part_one_example_4():
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

def test_part_one_example_5():
    lines = [
        '########################',
        '#@..............ac.GI.b#',
        '###d#e#f################',
        '###A#B#C################',
        '###g#h#i################',
        '########################',
    ]
    assert part_one(lines) == 81
