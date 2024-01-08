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


def part_one(lights_on, lights_off):
    """ part one """
    def step(on, off):
        new_on = set()
        new_off = set()
        for light in on:
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


def part_two(on, off):
    """ part two """
    return 'todo'


def main():
    """ main """
    lights_on = set()
    lights_off = set()
    row = 0
    for line in sys.stdin:
        line = line.replace('\n', '')
        for col in find_occurences('#', line):
            lights_on.add((row, col))
        for col in find_occurences(r'\.', line):
            lights_off.add((row, col))

        row += 1

    print('part_one', part_one(lights_on, lights_off))

    print('part_two', part_two(lights_on, lights_off))


main()
