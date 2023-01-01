# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

directions = {
        'N': (0, 1),
        'S': (0, -1),
        'E': (1, 0),
        'W': (-1, 0),
        }

NESW = 'NESW'


def turn(instruction, facing):
    idx = NESW.index(facing)
    match instruction:
        case ('R', degrees):
            idx = (idx + (degrees // 90)) % 4
        case ('L', degrees):
            idx = (idx - (degrees // 90)) % 4

    return NESW[idx]


def part_one(instructions):
    """ part one """
    facing = 'E'
    x, y = 0, 0
    for instruction in instructions:
        i, num = instruction
        if i in 'RL':
            facing = turn(instruction, facing)
            continue

        if i in 'F':
            direction = directions[facing]
        elif i in NESW:
            direction = directions[i]
        else:
            assert False

        dx, dy = direction
        x = x + num * dx
        y = y + num * dy

    return abs(x) + abs(y)


def rotate(instruction, x, y):
    match instruction:
        case ('R', 90) | ('L', 270):
            return y, -x
        case ('R', 180) | ('L', 180):
            return -x, -y
        case ('R', 270) | ('L', 90):
            return -y, x
    return x, y


def part_two(instructions):
    """ part two """
    x, y = 0, 0
    wx, wy = 10, 1
    for instruction in instructions:
        i, num = instruction

        if i in 'RL':
            wx, wy = rotate(instruction, wx, wy)

        elif i in NESW:
            dx, dy = directions[i]
            wx = wx + num * dx
            wy = wy + num * dy

        elif i == 'F':
            x = x + num * wx
            y = y + num * wy

        else:
            assert False

    return abs(x) + abs(y)


def main():
    """ main """
    instructions = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        instruction = (line[0], int(line[1:]))
        instructions.append(instruction)

    print('part_one', part_one(instructions))
    print('part_two', part_two(instructions))


main()
