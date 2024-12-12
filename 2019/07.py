"""Day 7: Amplification Circuit."""
import sys
from typing import Callable
from itertools import permutations

def computer(program: list[int], input: Callable[[], int], output:Callable[[int], None]) -> None:
    """ part one """
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

    i = 0 # instruction pointer
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
            write(i, 1, input())
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
            ValueError('Unknown opcode {}'.format(op))

def amplifiers(acs: list[int], phase_setting: list[int]) -> int:
    """Run the 5 amplifiers with the given phase setting."""

    values = phase_setting[::-1]
    values.insert(-1, 0)

    def input()->int:
        return values.pop()

    def output(value:int):
        values.insert(-1, value)

    for _ in range(5):
        computer(acs.copy(), input, output)

    return values.pop()


def part_one(acs: list[int]) -> tuple[int, list[int]]:
    """Find maximum signal that can be sent to the thrusters."""
    signal = 0
    phase_setting = []

    for perm in permutations(range(5)):
        setting = list(perm)
        output = amplifiers(acs.copy(), setting)
        if output > signal:
            signal = output
            phase_setting = setting

    return signal, phase_setting


def part_two(acs: list[int]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    acs: list[int] = [] # amplifier controller software
    for line in sys.stdin:
        line = line.strip()
        line = list(map(int, line.split(',')))
        acs.extend(line)

    print('part_one', part_one(acs.copy()))

    print('part_two', part_two(acs))

if __name__ == '__main__':
    main()

# part one tests
def test_amplifiers1():
    assert amplifiers([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0], [4,3,2,1,0]) == 43210
def test_amplifiers2():
    assert amplifiers([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0], [0,1,2,3,4]) == 54321
def test_amplifiers3():
    assert amplifiers([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0], [1,0,4,3,2])
def test_part_one_1():
    assert part_one([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) == (43210, [4,3,2,1,0 ])
def test_part_one_2():
    assert part_one([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) == (54321, [0,1,2,3,4 ])
def test_part_one_3():
    assert part_one([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) == (65210, [1,0,4,3,2 ])
