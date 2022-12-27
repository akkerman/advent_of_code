# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def part_one(lines):
    """ part one """
    idx = 0
    acc = 0
    done = set()
    while idx < len(lines):
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
            return acc, 'break'

        done.add(idx)
        acc = new_acc

    return acc, 'finish'


def part_two(lines):
    """ part two """
    for i, (instr, num) in enumerate(lines):
        copy = lines[:]
        if instr == 'nop':
            copy[i] = ('jmp', num)
        elif instr == 'jmp':
            copy[i] = ('nop', num)
        else:
            continue

        result, f = part_one(copy)
        # print(result, f)
        if f == 'finish':
            return result, f

    return None

def main():
    """ main """
    lines = []
    for line in sys.stdin:
        instr, num = line.replace('\n', '').split(' ')
        lines.append((instr, int(num)))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
