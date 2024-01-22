# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from utils import find_occurences

def neighbours(light):
    r,c = light
    return set([
       (r-1, c-1),
       (r-1, c  ),
       (r-1, c+1),

       (r  , c+1),
       (r  , c-1),

       (r+1, c-1),
       (r+1, c  ),
       (r+1, c+1),
    ])


def solve(lights_on, lights_off, stuck=set()):
    def step(on, off):
        new_on = set()
        new_off = set()
        for light in on:
            if light in stuck:
                new_on.add(light)
                continue

            nbs = neighbours(light)
            intersection = on & nbs
            if len(intersection) in [2,3]:
                new_on.add(light)
            else:
                new_off.add(light)

        for light in off:
            nbs = neighbours(light)
            intersection = on & nbs
            if len(intersection) == 3:
                new_on.add(light)
            else:
                new_off.add(light)

        return new_on, new_off


    on = lights_on
    off = lights_off

    for _ in range(100):
        on, off = step(on, off)

    return len(on)


def main():
    """ main """
    lights_on = set()
    lights_off = set()
    row = 0
    max_col = 0
    for line in sys.stdin:
        line = line.replace('\n', '')
        max_col = len(line) - 1
        for col in find_occurences('#', line):
            lights_on.add((row, col))
        for col in find_occurences(r'\.', line):
            lights_off.add((row, col))

        row += 1

    max_row = row - 1

    stuck = set([
        (0,0), (0, max_col),
        (max_row, 0), (max_row, max_col)
        ])


    print('part_one', solve(lights_on, lights_off))

    lights_on = lights_on | stuck
    lights_off = lights_off - stuck
    print('part_two', solve(lights_on, lights_off, stuck))


main()
