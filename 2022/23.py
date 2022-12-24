# pylint: disable=missing-module-docstring
# pylint: disable=invalid-name

import sys

directions_to_check = {
    'A': [(-1, -1), (-1, 0), (-1, 1),
          (1, -1), (1, 0), (1, 1),
          (0, -1),
          (0, 1),
          ],

    'N': [(-1, -1), (-1, 0), (-1, 1)],
    'S': [(1, -1), (1, 0), (1, 1)],
    'W': [(-1, -1), (0, -1), (1, -1)],
    'E': [(-1, 1), (0, 1), (1, 1)],
}

directions_to_move = {
    'N': (-1, 0),
    'S': (1, 0),
    'W': (0, -1),
    'E': (0, 1),
}

directions = ['N', 'S', 'W', 'E']


def moves_to_check(elf, direction: str):
    """ generate coordinates an elf needs to check before moving """
    r, c = elf
    return {(r + dr, c + dc) for (dr, dc) in directions_to_check[direction]}


def move_direction(elf, direction: str):
    """ move the elf in the indicated direction """
    r, c = elf
    dr, dc = directions_to_move[direction]
    return (r + dr, c + dc)


def calc_empty_tiles(elves):
    """ calculate empty tiles in the bounding box formed by the elves """
    minR = min(r for r, _ in elves)
    maxR = max(r for r, _ in elves)
    minC = min(c for _, c in elves)
    maxC = max(c for _, c in elves)

    return (maxR - minR + 1) * (maxC - minC + 1) - len(elves)


def part_one(elves):
    """ get number of free squares after 10 rounds """

    for _ in range(10):
        proposals = set()
        collisions = set()

        # first half, generate proposals and collisions
        for elf in elves:
            if not elves & moves_to_check(elf, 'A'):
                # wants to stay
                continue

            for direction in directions:
                if not elves & moves_to_check(elf, direction):
                    prop = move_direction(elf, direction)
                    if prop in proposals:
                        collisions.add(prop)
                    proposals.add(prop)
                    break

        # second half, move elves that don't collide
        moved_elves = set(elves)
        for elf in elves:
            if not elves & moves_to_check(elf, 'A'):
                # wants to stay
                continue

            for direction in directions:
                if not elves & moves_to_check(elf, direction):
                    prop = move_direction(elf, direction)
                    if prop not in collisions:
                        moved_elves.remove(elf)
                        moved_elves.add(prop)
                    break

        elves = moved_elves

        # the first direction the Elves considered
        # is moved to the end of the list of directions
        directions.append(directions.pop(0))

    return calc_empty_tiles(elves)


def part_two(lines):
    """ part_two """
    return 'todo'


def elves_as_coords(lines):
    """ translate the input to a set of coordinates """
    elves = set()
    for r, line in enumerate(lines):
        for c, entry in enumerate(line):
            if entry == '#':
                elves.add((r, c))

    return elves


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    elves = elves_as_coords(lines)

    print('part_one', part_one(elves))

    print('part_two', part_two(lines))


main()
