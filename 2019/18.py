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
    keys = set[Coord]()
    doors = set[Coord]()
    start: Coord = (-1, -1)
    for row, line in enumerate(lines):
        walls.update((row, col) for col, char in enumerate(line) if char == '#')
        keys.update((row, col) for col, char in enumerate(line) if char.islower())
        doors.update((row, col) for col, char in enumerate(line) if char.isupper())
        if '@' in line:
            start = (row, line.index('@'))

    return walls, keys, doors, start

directions = [(0, 1), (0,-1), (1, 0), (-1, 0 )]

def part_one(lines:list[str]) -> int:
    """Solution to part one."""
    walls, keys, doors, start = parse_input(lines)

    def next_steps(coord: Coord): 
        r,c = coord
        return [nxt for nxt in ((r, c+1), (r,c-1), (r+1, c), (r-1, c )) if nxt not in walls]

    collected_keys = set[Coord]()
    visited = {}
    # while collected_keys != keys:
    #     pass



    



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
