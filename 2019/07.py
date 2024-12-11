"""Day 7: Amplification Circuit."""
import sys
import string


def output(value:int):
    print(value)

def computer(program: list[int], input:int=1):
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
            ValueError('Unknown opcode {}'.format(op))

def part_one(lines):
    """Solution to part one."""
    return 'todo'


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = []
    for line in sys.stdin:
        line = line.strip()
        line = list(map(int, line.split(',')))
        
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
