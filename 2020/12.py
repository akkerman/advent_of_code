# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

directions = {
        'N': (0, -1),
        'S': (0, 1),
        'E': (1, 0),
        'W': (-1, 0),
        }

NESW = 'NESW'


def turn(instruction, facing):
    direction = NESW.index(facing)
    match instruction:
        case ('R', num):
            n = (direction + (num // 90)) % 4
            return NESW[n]
        case ('L', num):
            n = (direction - (num // 90)) % 4
            return NESW[n]
        case _:
            return facing


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

    return abs(x + y)


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    instructions = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        instruction = (line[0], int(line[1:]))
        instructions.append(instruction)

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))

    print(turn(('L', 90), 'S'))
main()
