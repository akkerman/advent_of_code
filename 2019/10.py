"""Day 10: Monitoring Station."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
Grid = set[Coord]

def part_one(asteroids:Grid):
    """Solution to part one."""
    print(asteroids)
    return 'todo'


def part_two(lines):
    """Solution to part two."""
    return 'todo'

def parse(lines: list[str]) -> Grid:
    asteroids: Grid = set()
    for row, line in enumerate(lines):
        asteroids.update((col, row) for col, char in enumerate(line) if char == '#')
    return asteroids


def main():
    """Parse input file, pass to puzzle solvers."""
    asteroids: Grid = set()
    row = 0
    for line in fileinput.input():
        line = line.strip()
        asteroids.update((col, row) for col, char in enumerate(line) if char == '#')

        

    print('part_one', part_one(asteroids))

    print('part_two', part_two(map))


if __name__ == '__main__':
    main()


def test_parse():
    input: list[str] = """#..
.#.
..#""".splitlines() # type: ignore
    assert parse(input) == {(0, 0), (1, 1), (2,2)}

def test_part_one_small():
    input: list[str] = """.#..#
.....
#####
....#
...##""".splitlines() # type: ignore
    assert part_one(parse(input)) == ((3,4), 8)
#
# def test_part_one_larger1():
#     input = """......#.#.
# #..#.#....
# ..#######.
# .#.#.###..
# .#..#.....
# ..#....#.#
# #..#....#.
# .##.#..###
# ##...#..#.
# .#....####"""
#     grid = parse(input.splitlines())
#     assert part_one(grid) == ((5,8), 33)
#
