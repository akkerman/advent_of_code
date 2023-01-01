# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def part_one(lines):
    """ part one """
    start = int(lines[0])
    busses = [int(i) for i in lines[1].split(',') if i != 'x']
    diff, bus = min((b - start % b, b) for b in busses)
    return bus * diff


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
