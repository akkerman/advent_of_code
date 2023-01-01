# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

def part_one(busses, departure):
    diff, bus = min((b - departure % b, b) for b in busses)
    return bus * diff


def part_two(busses):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    departure = int(lines[0])
    busses = [int(i) for i in lines[1].split(',') if i != 'x']


    print('part_one', part_one(busses, departure))

    print('part_two', part_two(busses))


main()
