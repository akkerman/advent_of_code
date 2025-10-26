"""Day 18: Like a Rogue."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer



traps = [ '^^.', '.^^', '^..', '..^' ]


def next_row(current_row:str) -> str:
    """Generate the next row based on current row."""
    nr = ''
    width = len(current_row)
    for i in range(width):
        if i == 0:
            fragment = '.' + current_row[i:i+2]
        elif i == width - 1:
            fragment = current_row[i-1:i+1] + '.'
        else:
            fragment = current_row[i-1:i+2]

        nr += '^' if fragment in traps else '.'

    return nr


def part_one(row:str):
    """Solution to part one."""
    nr = row
    cnt = Counter(nr)
    for _ in range(39):
        nr = next_row(nr)
        cnt.update(nr)
    return cnt['.']

def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines:list[str] = []
    for line in fileinput.input():
        line = line.strip()
        
        lines.append(line)

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
