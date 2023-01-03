# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def neigbours_3d(cube):
    d = (0, -1, 1)
    x, y, z = cube
    nbs = {(x+dx, y+dy, z+dz) for dx in d for dy in d for dz in d}
    return nbs - {cube}


def neigbours_4d(cube):
    d = (0, -1, 1)
    x, y, z, w = cube
    nbs = {(x+dx, y+dy, z+dz, w+dw) for dx in d for dy in d for dz in d for dw in d}
    return nbs - {cube}


def solve(initial_active_cubes, get_neigbours):
    active_cubes = set(initial_active_cubes)
    for _ in range(6):
        inactive_cubes = set()
        new_active_cubes = set(active_cubes)

        for active in active_cubes:
            nbs = get_neigbours(active)
            active_nbs = nbs & active_cubes
            inactive_nbs = nbs - active_nbs
            inactive_cubes |= inactive_nbs
            if len(active_nbs) in (2, 3):
                pass  # remains active
            else:
                new_active_cubes.remove(active)

        for inactive in inactive_cubes:
            nbs = get_neigbours(inactive)
            active_nbs = nbs & active_cubes
            if len(active_nbs) == 3:
                new_active_cubes.add(inactive)

        active_cubes = new_active_cubes
    return len(active_cubes)


def main():
    """ main """
    active_cubes_3d = set()
    active_cubes_4d = set()
    x = 0
    for x, line in enumerate(sys.stdin):
        for y, c in enumerate(line.replace('\n', '')):
            if c == '#':
                active_cubes_3d.add((x, y, 0))
                active_cubes_4d.add((x, y, 0, 0))

    print('part_one', solve(active_cubes_3d, neigbours_3d))

    print('part_two', solve(active_cubes_4d, neigbours_4d))


main()
