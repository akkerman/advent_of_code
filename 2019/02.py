# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

def part_one(program: List[int]) -> int:
    """ part one """
    program[1] = 12
    program[2] = 2
    for i in range(0, len(program), 4):
        if program[i] == 99:
            break
        op, x, y, z = program[i:i + 4]
        if op == 1:
            program[z] = program[x] + program[y]
            continue
        if op == 2:
            program[z] = program[x] * program[y]
            continue
    return program[0]


def part_two(program: List[int]) -> int:
    """ part two """
    return -1


def main():
    """ main """
    program: List[int] = []
    for line in sys.stdin:
        program = [int(x) for x in line.split(',')]
        break
        

    print('part_one', part_one(program.copy()))

    print('part_two', part_two(program.copy()))


main()

