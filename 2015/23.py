# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

def compute(instructions, a=0):
    registers = {
        'a' : a,
        'b' : 0,
    }

    idx = 0
    while 0 <= idx < len(instructions):
        if len(instructions[idx]) == 3:
            instr, r, pos = instructions[idx]

            if instr == 'jio' and registers[r] == 1:
                idx += pos
            elif instr == 'jie' and registers[r] % 2 == 0:
                idx += pos
            else:
                idx += 1

            continue


        assert len(instructions[idx]) == 2

        instr, r = instructions[idx]

        if instr == 'jmp':
            idx += r
            continue

        if instr == 'inc':
            registers[r] += 1
        elif instr == 'hlf':
            registers[r] = registers[r] // 2
        elif instr == 'tpl':
            registers[r] *= 3
        else:
            assert False

        idx += 1


    return registers['a'], registers['b']






def part_one(instructions):
    """ part one """
    _,b = compute(instructions)
    return b


def part_two(instructions):
    """ part two """
    _,b = compute(instructions, a=1)
    return b


def main():
    """ main """
    instructions = []
    def tryint(string):
        try:
            return int(string)
        except ValueError:
            return string

    for line in sys.stdin:
        line = line.replace('\n', '')
        line = re.split(r',? ', line)
        line = [tryint(p) for p  in line]
        instructions.append(line)

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


main()
