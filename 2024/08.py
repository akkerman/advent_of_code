# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import Dict, Tuple, Set


Antenna = Tuple[str, int, int]
Coord = Tuple[int, int]

def print_map(antennas: Set[Antenna], antinodes: Set[Coord], size: Tuple[int, int]):
    """ print map """
    coords_to_name: Dict[Coord, str] = {(r,c): n for n,r,c in antennas}
    for r in range(1, 1+size[0]):
        for c in range(1, 1+size[1]):
            if (r,c) in antinodes:
                print('#', end='')
            elif (r,c) in coords_to_name:
                print(coords_to_name[(r,c)], end='')
            else:
                print('.', end='')
        print()


def part_one(antennas: Set[Antenna], names: Set[str], size: Tuple[int, int]):
    """ part one """
    antinodes: Set[Coord] = set()

    def add_antinode(r:int,c:int):
        if 1 <= r <= size[0] and 1 <= c <= size[1]:
            antinodes.add((r,c))


    for name in names:
        coords = [(r,c) for n,r,c in antennas if n == name]
        for r1, c1 in coords:
            for r2, c2 in coords:
                if (r1, c1) == (r2, c2):
                    continue
                add_antinode(r1 + (r1-r2), c1 + (c1-c2))
                add_antinode(r2 + (r2-r1), c2 + (c2-c1))

    return len(antinodes)


def part_two(antennas: Set[Antenna], names: Set[str], size: Tuple[int, int]):
    """ part two """
    antinodes: Set[Coord] = set()

    def add_antinode(r:int,c:int):
        if 1 <= r <= size[0] and 1 <= c <= size[1]:
            antinodes.add((r,c))
            return True
        return False


    for name in names:
        coords = [(r,c) for n,r,c in antennas if n == name]
        for r1, c1 in coords:
            for r2, c2 in coords:
                if (r1, c1) == (r2, c2):
                    continue
                m = 1
                while add_antinode(r1 + (r2-r1)*m, c1+(c2-c1)*m):
                    m += 1
                m2 = 1
                while add_antinode(r2 + (r1-r2)*m2, c2+(c1-c2)*m2):
                    m2 += 1

    return len(antinodes)


def main():
    """ main """
    antennas: Set[Antenna] = set()
    names: Set[str] = set()
    rows = 0
    cols = 0
    for line in sys.stdin:
        line = line.strip()
        cols = len(line)
        rows += 1

        for col, name in enumerate(line):
            if name != '.':
                antennas.add((name, rows, 1+col))
                names.add(name)
        

    print('part_one', part_one(antennas, names, (rows, cols)))

    print('part_two', part_two(antennas, names, (rows, cols)))


main()
