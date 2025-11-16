"""2017 Day 19: A Series of Tubes"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
import string

Coord = tuple[int, int]

vert_dir = [(0, -1), (0, 1)]
horz_dir = [(-1, 0), (1, 0)]

def add(p: Coord, q: Coord):
    px,py = p
    qx,qy = q
    return (px+qx,py+qy)

def part_one(diagram: dict[Coord, str], start: Coord):
    """Solution to part one."""

    letters: list[str] = []
    pos = start
    direction = (0, 1)  # initially going down 

    while True:
        if pos not in diagram:
            return "".join(letters)
        char = diagram[pos]

        if char == '+':
            change = horz_dir if direction in vert_dir else vert_dir
            nbs = [add(pos, d) for d in change]
            for i, nb in enumerate(nbs):
                if nb in diagram:
                    pos = nb
                    direction = change[i]

            continue

        if char in string.ascii_uppercase:
            letters.append(char)
            
        pos = add(pos,direction)



def part_two(diagram: dict[Coord, str]):
    """Solution to part two."""
    return 'todo'

def print_diagram(diagram: dict[Coord,str]):
    for y in range(8):
        for x in range(18):
            pos = (x,y)
            if pos in diagram:
                print(diagram[pos], end='')
            else:
                print(' ', end='')
        print('')

def main():
    """Parse input file, pass to puzzle solvers."""
    diagram: dict[Coord, str] = {}

    start: Coord = (0, 0)
    for y, line in enumerate(fileinput.input()):
        for x, char in enumerate(line.strip('\n')):
            if char == ' ':
                continue
            diagram[(x, y)] = char
            if y == 0 and char == '|':
                # there is only one entry point on the top row
                start = (x, y)
      
    print('part_one', part_one(diagram, start))

    print('part_two', part_two(diagram))


if __name__ == '__main__':
    main()
