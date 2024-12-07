# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Tuple

Equation = Tuple[int, List[int]]

def can_be_true(value:int, numbers:List[int], concat:bool=False):
    def fn(value:int, current: int, numbers:List[int], formule:str=''):
        if current == value and len(numbers) == 0:
            return True
        if current > value:
            return False
        if len(numbers) == 0:
            return False
        if fn(value, current + numbers[0], numbers[1:], formule+'+'+str(numbers[0])):
            return True
        if fn(value, current * numbers[0], numbers[1:], formule+'*'+str(numbers[0])):
            return True
        if concat:
            if fn(value, int(str(current) + str(numbers[0])), numbers[1:], formule+'||'+str(numbers[0])):
                return True
        return False
    return fn(value, numbers[0], numbers[1:], ''+str(numbers[0]))

def part_one(equations:List[Equation]) -> int:
    """ part one """
    sum = 0
    for value, numbers in equations:
        if can_be_true(value, numbers):
            sum += value
    return sum

def part_two(equations:List[Equation]) -> int:
    """ part two """
    sum = 0
    for value, numbers in equations:
        if can_be_true(value, numbers, concat=True):
            sum += value
    return sum

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
