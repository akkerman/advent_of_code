# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

def input(): 
    return 1

def output(value:int):
    print('diagnostic:', value)

def computer(program: List[int]):
    """ part one """
    modes = [0,0,0,0]

    def set_value(instruction_pointer:int, parameter:int, value:int):
        # parameters that an instruction writes to wil never be in immediate mode
        program[program[instruction_pointer+parameter]] = value

    def get_value(instruction_pointer:int, parameter:int):
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
        if op == 99:
            break
        elif op == 1:
            set_value(i, 3, get_value(i, 1) + get_value(i, 2))
            i+=4
        elif op == 2:
            set_value(i, 3, get_value(i, 1) * get_value(i, 2))
            i+=4
        elif op == 3:
            set_value(i, 1, int(input()))
            i+=2
        elif op == 4:
            output(get_value(i, 1))
            i+=2
        else:
            ValueError('Unknown opcode {}'.format(op))


def part_one(program: List[int]):
    """ part one """
    return computer(program)

def part_two(program: List[int]):
    """ part two """
    return 'todo'

def main():
    """ main """

    program: List[int] = []
    for line in sys.stdin:
        line = line.strip()
        program = list(map(int, line.split(',')))
        

    print('part_one', part_one(program.copy()))

    print('part_two', part_two(program.copy()))


main()
