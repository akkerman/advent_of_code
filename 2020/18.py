# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque

def calculate(equation):
    operator = ''
    result = ''
    while equation:
        part = equation.popleft()
        match part:
            case '+' | '*':
                operator = part
            case '(':
                intermediate = calculate(equation)
                if operator:
                    result = str(eval(result + operator + intermediate))
                elif equation:
                    result = intermediate
                else:
                    return intermediate
            case ')':
                return result
            case _:
                if operator:
                    result = str(eval(result + operator + part))
                else:
                    result = part
    return result





def part_one(lines):
    """ part one """
    total = 0
    for line in lines:
        result = calculate(deque(line))
        total += int(result)

    return total


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '').replace('(', '( ').replace(')', ' )').split(' ')
        lines.append(line)


    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
