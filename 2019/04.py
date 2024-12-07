# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import Counter

def is_valid(password: str) -> bool:
    """ check if password is ascending and has at least one pair """
    inc = all(x <= y for x, y in zip(password, password[1:]))
    eq = any(x == y for x, y in zip(password, password[1:]))

    return inc and eq

def part_one(start:int, end:int) -> int:
    """ Count all valid passwords in range """
    return sum(is_valid(password) for password in map(str, range(start, end + 1)))

def part_two(start:int, end:int):
    """ Count all valid passwords in range with at least one pair """
    return sum(
        is_valid(password) and 2 in Counter(password).values()
        for password in map(str, range(start, end + 1))
    )



def main():
    """ main """
    start = 0
    end = 0
    for line in sys.stdin:
        line = line.replace('\n', '')
        start,end = [int(x) for x in line.split('-')]
        

    print('part_one', part_one(start, end))

    # too low: 1020
    print('part_two', part_two(start, end))


main()
