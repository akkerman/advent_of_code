"""2017 Day 18: Duet"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

re_set = re.compile(r'set (.+) (.+)')
re_add = re.compile(r'add (.+) (.+)')
re_mul = re.compile(r'mul (.+) (.+)')
re_mod = re.compile(r'mod (.+) (.+)')
re_snd = re.compile(r'snd (.+)')
re_rcv = re.compile(r'rcv (.+)')
re_jgz = re.compile(r'jgz (.+) (.+)')


def solve(lines: list[str]):
    """Solve the puzzle."""
    registers:dict[str,int] = defaultdict(int)
    idx = 0
    last_sound = 0

    def valueof(x: str) -> int:
        try:
            return int(x)
        except ValueError:
            return registers[x]

    while True:
        line = lines[idx]
        # print(f'idx {idx} line: {line} registers: {registers}')
        if m := re_set.match(line):
            x, y = m.groups()
            registers[x] = valueof(y)
            idx += 1
            continue
        elif m := re_add.match(line):
            x, y = m.groups()
            registers[x] += valueof(y)
            idx += 1
            continue
        elif m := re_mul.match(line):
            x, y = m.groups()
            registers[x] *= valueof(y)
            idx += 1
            continue
        elif m := re_mod.match(line):
            x, y = m.groups()
            registers[x] %= valueof(y)
            idx += 1
            continue
        elif m := re_snd.match(line):
            x = m.group(1)
            try:
                last_sound = int(x)
            except ValueError:
                last_sound = registers[x]
            # print(f'snd {last_sound}')
            idx += 1
            continue
        elif m := re_rcv.match(line):
            x = m.group(1)
            if registers[x] != 0:
                return last_sound
            idx += 1
            continue
        elif m := re_jgz.match(line):
            x, y = m.groups()
            x_val = valueof(x)
            y_val = valueof(y)
            if x_val > 0:
                idx += y_val
            else:
                idx += 1
        else:
            raise ValueError(f'Unrecognized line: {line}')


def part_one(lines):
    """Solution to part one."""
    return solve(lines)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [line.strip() for line in fileinput.input()]




    # too low: 5985
    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
