# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
from typing import List, Tuple, Dict 

Equation = Tuple[int, List[int]]

def can_be_true(value:int, numbers:List[int]):
    def fn(value:int, current: int, numbers:List[int]):
        if current == value:
            return True
        if current > value:
            return False
        if len(numbers) == 0:
            return False
        if fn(value, current + numbers[0], numbers[1:]):
            return True
        if fn(value, current * numbers[0], numbers[1:]):
            return True
        return False
    return fn(value, numbers[0], numbers[1:])

def part_one(equations:List[Equation]):
    """ part one """
    sum = 0
    for value, numbers in equations:
        print(value, end=" ")
        if can_be_true(value, numbers):
            sum += value
            print('true')
        else:
            print('false')
    return sum


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    equations:List[Equation] = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        testvalue, nmbrs = line.split(': ')

        testvalue = int(testvalue)
        nmbrs = list(map(int, nmbrs.split(' ')))
        equations.append((testvalue, nmbrs))

    print('part_one', part_one(equations))

    print('part_two', part_two(equations))


main()
