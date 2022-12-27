# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def split_on(arr, on):
    curr = []
    for elem in arr:
        if elem == on:
            yield curr
            curr = []
        else:
            curr.append(elem)
    yield curr


def part_one(lines):
    """ part one """
    total = 0
    for group in split_on(lines, ''):
        s = set()
        for person in group:
            for answer in person:
                s.add(answer)
        total += len(s)

    return total


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
