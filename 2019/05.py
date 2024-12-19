# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List


def output(value:int):
    print('diagnostic:', value)

def computer(program: List[int], input:int=1):
    """Computer."""
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    HALT = 99

    modes = [0,0,0,0]

    def write(instruction_pointer:int, parameter:int, value:int):
        # parameters that an instruction writes to wil never be in immediate mode
        program[program[instruction_pointer+parameter]] = value

    def read(instruction_pointer:int, parameter:int):
        if modes[parameter]:
            # immediate mode
            return program[instruction_pointer + parameter]
        else:
            # position mode
            return program[program[instruction_pointer + parameter]]

    i = 0
    while i < len(program):
        p3, p2, p1, o1, o2 = list(str(program[i]).zfill(5))
        modes = [0, int(p1), int(p2), int(p3)]
        op = int(o1 + o2)
        if op == HALT:
            break
        elif op == ADD:
            write(i, 3, read(i, 1) + read(i, 2))
            i+=4
        elif op == MULTIPLY:
            write(i, 3, read(i, 1) * read(i, 2))
            i+=4
        elif op == INPUT:
            write(i, 1, input)
            i+=2
        elif op == OUTPUT:
            output(read(i, 1))
            i+=2
        elif op == JUMP_IF_TRUE:
            if read(i, 1) != 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == JUMP_IF_FALSE:
            if read(i, 1) == 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == LESS_THAN:
            write(i, 3, 1 if read(i, 1) < read(i, 2) else 0)
            i+=4
        elif op == EQUALS:
            write(i, 3, 1 if read(i, 1) == read(i, 2) else 0)
            i+=4
        else:
            raise ValueError('Unknown opcode {}'.format(op))


def part_one(program: List[int]):
    """ part one """
    computer(program)

def part_two(program: List[int]):
    """ part two """
    computer(program, 5)

def main():
    """ main """

    program: List[int] = []
    for line in sys.stdin:
        line = line.strip()
        program = list(map(int, line.split(',')))
        

    print('\npart_one')
    part_one(program.copy())

    print('\npart_two')
    part_two(program.copy())


main()
