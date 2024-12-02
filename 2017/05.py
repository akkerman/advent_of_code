# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List


def part_one(jumps: List[int]) -> int:
    """ part one """
    idx = 0
    step = 0
    while True:
        if idx < 0 or idx >= len(jumps):
            return step

        jmp = jumps[idx]
        jumps[idx] += 1
        idx += jmp
        step += 1

def part_two(jumps: List[int]) -> int:
    """ part two """
    idx = 0
    step = 0
    while True:
        if idx < 0 or idx >= len(jumps):
            return step

        jmp = jumps[idx]

        if jmp >= 3:
            jumps[idx] -= 1
        else:
            jumps[idx] += 1

        idx += jmp
        step += 1


def main():
    """ main """
    jumps: List[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        
        jumps.append(int(line))

    jumps_copy = jumps.copy()
    print('part_one', part_one(jumps_copy))

    print('part_two', part_two(jumps))


main()
