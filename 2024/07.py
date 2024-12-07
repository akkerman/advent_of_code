# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
from typing import List, Tuple, Dict 


def part_one(lines):
    """ part one """
    return 'todo'


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines:List = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
