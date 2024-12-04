# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

from util_09 import remove_garbage, score_groups, take_garbage

def part_one(line:str) -> int:
    """ part one """
    return score_groups(remove_garbage(line))

def part_two(line:str) -> int:
    """ part two """
    return len(take_garbage(line))

def main():
    """ main """
    lines: List[str] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    # too low 2424
    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))

main()
