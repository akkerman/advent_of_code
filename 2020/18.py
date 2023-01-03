# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
# pylint: disable=eval-used

import sys
from collections import deque


def calculate(expression):
    operator = ''
    result = ''
    while expression:
        part = expression.popleft()
        match part:
            case '+' | '*':
                operator = part
            case '(':
                intermediate = calculate(expression)
                expression.appendleft(intermediate)
            case ')':
                return result
            case _:
                if operator:
                    result = str(eval(result + operator + part))
                else:
                    result = part
    return result


def calculate2(expression, prev=None):
    operator = ''
    result = ''
    while expression:
        part = expression.popleft()
        match part:
            case '+':
                operator = part
            case '*':
                operator = part
                intermediate = calculate2(expression)
                expression.appendleft(intermediate)
            case '(':
                intermediate = calculate2(expression, '(')
                expression.appendleft(intermediate)
            case ')':
                if prev != '(':
                    expression.appendleft(')')
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
    total = 0
    for line in lines:
        result = calculate2(deque(line))
        total += int(result)

    return total


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '').replace('(', '( ').replace(')', ' )').split(' ')
        lines.append(line)


    print('part_one', part_one(lines))

    print('part_two', part_two(lines))

    # too high: 168775135619137


main()
