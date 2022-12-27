# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def part_one(lines):
    """ part one """
    idx = 0
    acc = 0
    done = set()
    while True:
        instr, num = lines[idx]

        new_acc = acc
        if instr == 'nop':
            idx += 1
        elif instr == 'acc':
            new_acc += num
            idx += 1
        elif instr == 'jmp':
            idx += num

        if idx in done:
            return acc

        done.add(idx)
        acc = new_acc


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        instr, num = line.replace('\n', '').split(' ')

        try:
            lines.append((instr, int(num)))
        except:
            lines.append((instr, num))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
