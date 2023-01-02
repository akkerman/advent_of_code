# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from math import lcm


def part_one(busses, departure):
    diff, bus = min((b - departure % b, b) for b in busses)
    return bus * diff


def solve(x, inc, numbers, remainders):
    # adaptation of chinese remainder theorem
    while True:
        x += inc
        for num, rem in zip(numbers, remainders):
            if (x + rem) % num != 0:
                break
        else:
            # did not break out of loop, all match
            return x


def part_two(busses):
    """ part two """
    num = []
    rem = []
    for i, bus in enumerate(busses):
        if bus == 'x':
            continue
        num.append(int(bus))
        rem.append(i)

    x = num[0]
    inc = num[0]


    for i, n in enumerate(num):
        x = solve(x, inc, num[0:i+1], rem[0:i+1])
        inc = lcm(inc, n)

    return x


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    departure = int(lines[0])
    busses = [int(i) for i in lines[1].split(',') if i != 'x']

    print('part_one', part_one(busses, departure))
    print('part_two', part_two(lines[1].split(',')))


main()
