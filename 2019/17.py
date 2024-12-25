"""Day 17: Set and Forget."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from intcode import computer, IO

SCAFFOLD = 35 # '#'
SPACE = 46 # '.'
NEWLINE = 10 # '\n'

class Vacuum(IO):
    def __init__(self):
        self.row = 0
        self.col = 0
        self.scaffolds = set[tuple[int, int]]()

    def input(self):
        return 1

    def output(self, value):
        if value == NEWLINE:
            self.row += 1
            self.col = 0
            return

        if value == SCAFFOLD:
            self.scaffolds.add((self.row, self.col))

        self.col += 1



def part_one(scaffolds: set[tuple[int, int]]):
    """Calculate the sum of the alignment parameters of the scaffold intersections."""
    intersections = list[tuple[int, int]]()
    for x, y in scaffolds:
        if (x+1, y) not in scaffolds:
            continue
        if (x-1, y) not in scaffolds:
            continue
        if (x, y+1) not in scaffolds:
            continue
        if (x, y-1) not in scaffolds:
            continue
        intersections.append((x, y))
    return sum(x * y for x, y in intersections)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    vacuum = Vacuum()

    computer(defaultdict(int, enumerate(program)), vacuum)

    print('part_one', part_one(vacuum.scaffolds))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
