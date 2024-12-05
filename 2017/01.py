# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

def rotate(l:List[int], n:int) -> List[int]:
    return l[n:] + l[:n]

def sum_pairs(l1:List[int], l2:List[int]) -> int:
    return sum([a for a, b in zip(l1,l2) if a == b])

def part_one(line:List[int]) -> int:
    """ part one """
    return sum_pairs(line, rotate(line, 1))


def part_two(line:List[int]) -> int:
    """ part two """
    return sum_pairs(line, rotate(line, len(line)//2))


def main():
    """ main """
    lines: List[List[int]] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = [int(i) for i in line]
        lines.append(line)

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))


main()
