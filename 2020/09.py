# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def part_one(lines):
    """ part one """
    for start, num in enumerate(lines[25:]):
        part = lines[start:start+25]

        if not [x+y for x in part for y in part if (x+y) == num]:
            return num


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(int(line))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
