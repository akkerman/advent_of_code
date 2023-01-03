# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def neigbours(cube):
    d = (0, -1, 1)
    x, y, z = cube
    nbs = {(x+dx, y+dy, z+dz) for dx in d for dy in d for dz in d}
    return nbs - {cube}


def part_one(initial_active_cubes):
    """ part one """
    active_cubes = set(initial_active_cubes)
    for _ in range(6):
        inactive_cubes = set()
        new_active_cubes = set(active_cubes)

        for active in active_cubes:
            nbs = neigbours(active)
            active_nbs = nbs & active_cubes
            inactive_nbs = nbs - active_cubes
            inactive_cubes |= inactive_nbs
            if len(active_nbs) in (2, 3):
                pass  # remains active
            else:
                new_active_cubes.remove(active)

        for inactive in inactive_cubes:
            nbs = neigbours(inactive)
            active_nbs = nbs & active_cubes
            if len(active_nbs) == 3:
                new_active_cubes.add(inactive)

        active_cubes = new_active_cubes
    return len(active_cubes)


def part_two(active_cubes):
    """ part two """
    return 'todo'


def main():
    """ main """
    active_cubes = set()
    x = 0
    for x, line in enumerate(sys.stdin):
        for y, c in enumerate(line.replace('\n', '')):
            if c == '#':
                active_cubes.add((x,y,0))

    print('part_one', part_one(active_cubes))

    print('part_two', part_two(active_cubes))


main()
