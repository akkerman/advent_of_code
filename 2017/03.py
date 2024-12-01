# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
from typing import List

def get_corner(n:int):
    m = int(n**0.5)
    if m % 2 == 0:
        m += 1
    while True:
        k = m**2
        if k >= n:
            return m, k
        m += 2



def part_one(target):
    """ part one """
    m, k = get_corner(target)

    corners: List[int] = [k, k-m+1, k-2*m+2, k-3*m+3, (m-2)**2]
    print(corners)
    if target in corners:
        return m

    centers: List[int] = []
    s = corners[0]
    for c in corners:
        centers.append(int((s+c)/2))
        s = c

    print(centers)
    if target in centers:
        return m // 2


    min_dist = m
    for c in centers:
        min_dist = min(min_dist, abs(c - target))

    return m//2 + min_dist


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines: List[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        
        lines.append(int(line))

    # too high: 710
    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))


main()
