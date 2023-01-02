# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

def part_one(lines):
    """ part one """
    mask = ''
    mem = {}
    for what, value in lines:
        if what == 'mask':
            mask = value
            continue

        mem[what] = overwrite_int(mask, value)

    return sum(mem.values())


def part_two(lines):
    """ part two """
    return 'todo'


def int_to_bits(num):
    return f"{int(num):36b}".replace(' ', '0')


def bits_to_int(bits):
    return int(bits, 2)


def overwrite(mask, value):
    return ''.join([y if x == 'X' else x for x, y in zip(mask, value)])

def overwrite_int(mask, value):
    return bits_to_int(overwrite(mask, int_to_bits(value)))


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '').split(' = ')
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))

main()
