# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def part_one(lines):
    """ part one """
    return sum(len(line) - len(eval(f'{line}')) for line in lines)

def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', part_one(lines))
    print('part_two', part_two(lines))


main()
