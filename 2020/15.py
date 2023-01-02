# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque


def part_one(numbers):
    """ part one """
    num_turns = len(numbers)
    turns = deque(reversed(numbers))
    last = turns.popleft()
    while num_turns < 2020:
        try:
            current = turns.index(last) + 1
        except:
            # not in list
            current = 0

        turns.appendleft(last)
        last = current

        num_turns += 1

    return last


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    numbers = [int(x) for x in lines[0].split(',')]

    print('part_one', part_one(numbers))

    print('part_two', part_two(lines))


main()
