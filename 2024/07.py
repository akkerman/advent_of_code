"""Day 7: Bridge Repair."""
import sys

Equation = tuple[int, list[int]]

def can_be_true(value:int, numbers:list[int], concat:bool=False):
    """Check if there is a way to get the value with the numbers interspersed by +, * and concatenation."""
    def fn(current: int, numbers:list[int], formule:str=''):
        if current == value and len(numbers) == 0:
            return True
        if current > value:
            return False
        if len(numbers) == 0:
            return False
        if fn( current + numbers[0], numbers[1:], formule+'+'+str(numbers[0])):
            return True
        if fn( current * numbers[0], numbers[1:], formule+'*'+str(numbers[0])):
            return True
        if concat:
            if fn(int(str(current) + str(numbers[0])), numbers[1:], formule+'||'+str(numbers[0])):
                return True
        return False
    return fn(numbers[0], numbers[1:], ''+str(numbers[0]))

def part_one(equations:list[Equation]) -> int:
    """Sum all values of equation that can be true."""
    return sum(value for value, numbers in equations if can_be_true(value, numbers))

def part_two(equations:list[Equation]) -> int:
    """Sum all values of equation that can be true with concatenation."""
    return sum(value for value, numbers in equations if can_be_true(value, numbers, concat=True))

def main():
    """Parse input file, pass to puzzle solvers."""
    equations:list[Equation] = []
    for line in sys.stdin:
        testvalue, nmbrs = line.strip().split(': ')

        testvalue = int(testvalue)
        nmbrs = list(map(int, nmbrs.split(' ')))
        equations.append((testvalue, nmbrs))

    print('part_one', part_one(equations))
    print('part_two', part_two(equations))

main()
