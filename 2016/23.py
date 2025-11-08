"""Day 23: Safe Cracking."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer


re_cpy = re.compile('cpy (.+) (.+)')
re_inc = re.compile('inc (.+)')
re_dec = re.compile('dec (.+)')
re_jnz = re.compile('jnz (.+) (.+)')

def solve(lines: list[str], c:int=0):
    idx = 0
    registers: dict[str,int] = defaultdict(int)
    registers['c'] = c

    while 0 <= idx < len(lines):
        line = lines[idx]
    
        if (m := re_cpy.match(line)):
            x, y = m.groups()
            try:
                x = int(x)
                registers[y] = x
            except ValueError:
                registers[y] = registers[x]
            idx += 1
            continue

        if (m := re_inc.match(line)):
            x = m.groups()[0]
            registers[x] += 1
            idx += 1
            continue

        if (m := re_dec.match(line)):
            x = m.groups()[0]
            registers[x] -= 1
            idx += 1
            continue

        if (m := re_jnz.match(line)):
            x, y = m.groups()
            try:
                value = int(x)
            except ValueError:
                value = registers[x]

            if value != 0:
                idx += int(y)
            else:
                idx += 1
            continue

    return registers['a']

def part_one(lines: list[str]) -> int:
    """Solution to part one."""
    return solve(lines)


def part_two(lines: list[str]) -> int:
    """Solution to part two."""
    return solve(lines, c=1)


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [
        line.strip()
        for line in fileinput.input()
        ]

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
