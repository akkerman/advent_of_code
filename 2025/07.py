"""2025 Day 7: Laboratories"""
import fileinput
from functools import lru_cache

Coord = tuple[int, int]

def part_one(splitters: set[Coord], start: Coord, bottom:int) -> int:
    """Solution to part one."""
    beams = set[Coord]()
    beams.add(start)
    split_count: int = 0

    while True:
        new_beams = set[Coord]()
        for x, y in beams:
            if (x, y+1) in splitters:
                new_beams.add((x-1, y+1))
                new_beams.add((x+1, y+1))
                split_count += 1
            else:
                new_beams.add((x, y+1))
        beams = new_beams
        if any(y >= bottom for _, y in beams):
            break
    return split_count

def part_two(splitters: set[Coord], start: Coord, bottom: int) -> int:
    """Solution to part two."""
    @lru_cache()
    def split_time(x:int, y:int) -> int:
        if y >= bottom:
            return 1

        if (x, y+1) in splitters:
            return split_time(x-1, y+1) + split_time(x+1, y+1)

        return split_time(x, y+1)

    return split_time(*start)


def main():
    """Parse input file, pass to puzzle solvers."""
    splitters = set[Coord]()
    start: Coord = (0, 0)

    for y, line in enumerate(fileinput.input()):
        line = line.strip()
        for x, c in enumerate(line):
            if c == 'S':
                start = (x, y)
            elif c == '^':
                splitters.add((x, y))
            else:
                assert c == '.'

    bottom = max(y for _, y in splitters) + 1
    print('part_one', part_one(splitters, start, bottom))

    print('part_two', part_two(splitters, start, bottom))


if __name__ == '__main__':
    main()
