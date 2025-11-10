"""2017 Day 16: Permutation Promenade"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

re_spin = re.compile(r's(\d+)')
re_exchange = re.compile(r'x(\d+)/(\d+)')
re_partner = re.compile(r'p([a-p])/([a-p])')

PROGRAM = [chr(ord('a') + i) for i in range(16)]

def part_one(moves: list[str], program: list[str]):
    """Solution to part one."""
    for move in moves:
        if m := re_spin.match(move):
            x = int(m.group(1))
            program = program[-x:] + program[:-x]
        elif m := re_exchange.match(move):
            a, b = int(m.group(1)), int(m.group(2))
            program[a], program[b] = program[b], program[a]
        elif m := re_partner.match(move):
            a, b = m.group(1), m.group(2)
            ia, ib = program.index(a), program.index(b)
            program[ia], program[ib] = program[ib], program[ia]
    return ''.join(program)


BILLION = 1_000_000_000
def part_two(moves: list[str], program: list[str]):
    """Solution to part two."""
    seen: list[str] = []
    for i in range(BILLION):
        state = ''.join(program)
        if state in seen:
            seen_at = seen.index(state)
            cycle_length = i - seen_at
            remaining = (BILLION - i) % cycle_length
            return seen[seen_at + remaining]
        seen.append(state)
        program = list(part_one(moves, program))
        
def main():
    """Parse input file, pass to puzzle solvers."""

    moves: list[str] = next(fileinput.input()).strip().split(',')

    print('part_one', part_one(moves, PROGRAM.copy()))

    print('part_two', part_two(moves, PROGRAM.copy()))


if __name__ == '__main__':
    main()

def test_spin():
    assert part_one(['s1'], list('abcde')) == 'eabcd'
def test_exchange():
    assert part_one(['x3/4'], list('eabcd')) == 'eabdc'
def test_partner():
    assert part_one(['pe/b'], list('eabdc')) == 'baedc'
