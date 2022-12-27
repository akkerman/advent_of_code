# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def part_one(lines):
    """ part one """
    return [a * b for a in lines for b in lines if (a + b) == 2020][0]


def part_two(lines):
    """ part two """
    return [a * b * c for a in lines for b in lines for c in lines if (a + b + c) == 2020][0]


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(int(line))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
