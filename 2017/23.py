"""2017 Day 23: Coprocessor Conflagration"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

re_set = re.compile(r'set (.) (-?.+)')
re_sub = re.compile(r'sub (.) (-?.+)')
re_mul = re.compile(r'mul (.) (-?.+)')
re_jnz = re.compile(r'jnz (.) (-?.+)')

def part_one(instructions:list[str])->int:
    """Solution to part one."""
    registers:dict[str,int] = defaultdict(int)
    idx = 0
    mul_count = 0

    def valueof(x: str) -> int:
        try:
            return int(x)
        except ValueError:
            return registers[x]

    while 0 <= idx < len(instructions):
        instruction = instructions[idx]
        if m := re_set.match(instruction):
            x, y = m.groups()
            registers[x] = valueof(y)
            idx += 1
            continue
        elif m := re_sub.match(instruction):
            x, y = m.groups()
            registers[x] -= valueof(y)
            idx += 1
            continue
        elif m := re_mul.match(instruction):
            x, y = m.groups()
            registers[x] *= valueof(y)
            mul_count += 1
            idx += 1
            continue
        elif m := re_jnz.match(instruction):
            x, y = m.groups()
            if valueof(x) != 0:
                idx += valueof(y)
            else:
                idx += 1
            continue
        else:
            raise ValueError(f'Unknown instruction: {instruction}')

    return mul_count




def part_two(instructions:list[str])->int:
    """Solution to part two."""

    a = 1
    b = 84 * 100 + 100000
    c = b + 17000
    h = 0

    while True:
        f = 1
        d = 2
        while True:
            if b % d == 0:
                f = 0
            d += 1
            if d * d <= b:
                break
        if f == 0:
            h += 1
        if b == c:
            break
        b += 17

    return h


def main():
    """Parse input file, pass to puzzle solvers."""
    instructions = [line.strip() for line in fileinput.input()]

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()
