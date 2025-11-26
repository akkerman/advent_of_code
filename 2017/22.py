"""2017 Day 22: Sporifica Virus"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
Dir = tuple[int, int]
Turn = str

U: Dir = (0, -1)
D: Dir = (0, 1)
L: Dir = (-1, 0)
R: Dir = (1, 0)

TURNS: dict[tuple[Dir, Turn], Dir] = {
        (U, 'L'): L,
        (U, 'R'): R,
        (D, 'L'): R,
        (D, 'R'): L,
        (R, 'L'): U,
        (R, 'R'): D,
        (L, 'L'): D,
        (L, 'R'): U,
        }
            

def move(pos: Coord, dir: Dir) -> Coord:
    """Move from pos in dir."""
    return (pos[0] + dir[0], pos[1] + dir[1])

def part_one(infected: set[Coord], start: Coord):
    """Solution to part one."""
    dir: Dir = U
    pos: Coord = start
    infections: int = 0

    for _ in range(10000):
        if pos in infected:
            dir = TURNS[(dir, 'R')]
            infected.remove(pos) # clean
        else:
            dir = TURNS[(dir, 'L')]
            infected.add(pos) # infect
            infections += 1

        pos = move(pos, dir)


    return infections


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    maze: set[Coord] = set()

    x:int = 1
    y:int = 1
    for line in fileinput.input():
        x = 1
        for char in line.strip():
            if char == '#':
                maze.add((x, y))
            x += 1
        y += 1


        
    center: Coord = (x // 2, y // 2)

    print('part_one', part_one(maze, center))

    print('part_two', part_two(maze))


if __name__ == '__main__':
    main()
