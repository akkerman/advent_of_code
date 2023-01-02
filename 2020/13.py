# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from math import lcm


def part_one(busses, departure):
    diff, bus = min((b - departure % b, b) for b in busses)
    return bus * diff


def chinese_remainder(numbers, remainders):
    x = 1
    while True:
        for num, rem in zip(numbers, remainders):
            if x % num != rem:
                break
        else:
            # did not break out of loop, all match
            return x

        x += 1


def part_two(busses):
    """ part two """
    num = []
    rem = []
    for i, bus in enumerate(busses):
        if bus == 'x':
            continue
        num.append(int(bus))
        rem.append(i)

    return chinese_remainder(num, rem)


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    departure = int(lines[0])
    busses = [int(i) for i in lines[1].split(',') if i != 'x']

    print('part_one', part_one(busses, departure))

    print('x,5,x,7 is        31', part_two('x,5,x,7'.split(',')))
    print('17,x,13,19 is   3417', part_two('17,x,13,19'.split(',')))
    print('67,7,59,61 is 754018', part_two('67,7,59,61'.split(',')))

    print('part_two', part_two(lines[1].split(',')))


main()
