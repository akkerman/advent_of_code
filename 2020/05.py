# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def find(low, hig, maxInt, bsp):
    pos = list(range(maxInt+1))
    for char in bsp:
        if char == low:
            pos = pos[:len(pos)//2]
        if char == hig:
            pos = pos[len(pos)//2:]

    return pos[0]


def seat(bsp):
    r = find('F', 'B', 127, bsp[0:-3])
    c = find('L', 'R', 8, bsp[-3:])
    # print('r,c', r, c)
    return 8 * r + c


def part_one(lines):
    """ part one """
    maxId = 0
    for bsp in lines:
        seatId = seat(bsp)
        maxId = max(maxId, seatId)
    return maxId


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
