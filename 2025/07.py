"""2025 Day 7: Laboratories"""
import fileinput
from functools import lru_cache

Coord = tuple[int, int]

def part_one(splitters: set[Coord], start: Coord):
    """Solution to part one."""
    bottom = max(y for _, y in splitters) + 1

    beams = set[Coord]()
    beams.add(start)


    splits: int = 0

    while True:
        new_beams = set[Coord]()
        for x, y in beams:
            if (x, y+1) in splitters:
                new_beams.add((x-1, y+1))
                new_beams.add((x+1, y+1))
                splits += 1
            else:
                new_beams.add((x, y+1))
        beams = new_beams
        if any(y >= bottom for _, y in beams):
            break
    return splits



def part_two(splitters: set[Coord], start: Coord):
    """Solution to part two."""
    bottom = max(y for _, y in splitters) + 1

    @lru_cache(maxsize=None)
    def split_time(timelines: int, ray: Coord) -> int:
        x, y = ray
        if y >= bottom:
            return 1 # count only the last timeline
        if (x, y+1) in splitters:
            return split_time(timelines + 1, (x-1, y+1)) + split_time(timelines + 1, (x+1, y+1))
        else:
            return split_time(timelines, (x, y+1))

    return split_time(0, start)


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
        


    print('part_one', part_one(splitters, start))

    print('part_two', part_two(splitters, start))


if __name__ == '__main__':
    main()
