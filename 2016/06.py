# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import Counter


def part_one(lines):
    """ part one """
    rotated = []
    for _ in range(len(lines[0])):
        rotated.append([])
    for line in lines:
        for i,c in enumerate(line):
            rotated[i].append(c)

    word=""
    for r in rotated:
        c = Counter(r)
        word += c.most_common(1)[0][0]

    return word


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
