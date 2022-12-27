# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import Counter


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
    total = 0
    for group in split_on(lines, ''):
        c = Counter()
        persons = 0
        for person in group:
            persons += 1
            for answer in person:
                c.update(answer)

        everyone = [answer for answer, count in c.items() if count == persons]
        total += len(everyone)

    return total


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
