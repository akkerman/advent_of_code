# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def part_one(lines):
    """ part one """
    for start, num in enumerate(lines[25:]):
        part = lines[start:start+25]

        if not [x+y for x in part for y in part if (x+y) == num]:
            return num


def part_two(lines):
    """ part two """
    needle = part_one(lines)
    totals = []
    total = 0
    start = 0
    end = len(lines)
    for idx, num in enumerate(lines):
        total += num
        totals.append(total)
        if total > needle:
            end = min(end, idx)

    while True:
        probe = totals[end] - totals[start]
        if probe == needle:
            break
        if probe < needle:
            end += 1
        if probe > needle:
            start += 1

    lst = sorted(lines[start:end])
    smallest = lst[0]
    largest = lst[-1]
    return smallest + largest


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(int(line))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
