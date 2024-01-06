# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

from operator import add

# https://www.redblobgames.com/grids/hexagons/
dirs = {
    "ne": ( 1, 0,-1),
    "e":  ( 1,-1, 0),
    "se": ( 0,-1, 1),
    "sw": (-1, 0, 1),
    "w" : (-1, 1, 0),
    "nw": ( 0, 1,-1),
}

def initial_state(lines):
    black_tiles = set()

    for line in lines:
        tile = (0,0,0)
        for step in line:
            d = dirs[step]
            tile = tuple(map(add, tile, d))

        if tile in black_tiles:
            black_tiles.remove(tile)
        else:
            black_tiles.add(tile)

    return black_tiles

def part_one(lines):
    """ part one """
    return len(initial_state(lines))


def part_two(lines):
    """ part two """
    return "todo"

def parse_directions(line):
    dirs = []

    m = len(line)
    i = 0

    while i < m:
        d = line[i]
        if i >= m - 1:
            dirs.append(d)
            break

        dd = line[i+1]

        if d in 'ns' and dd in 'we':
            dirs.append(d+dd)
            i += 2
        else:
            dirs.append(d)
            i += 1

    return dirs



def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(parse_directions(line))


    # for line in lines:
    #     print(",".join(line))



    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
