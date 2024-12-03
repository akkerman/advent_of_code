# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
import re


def part_one(memory:str)->int:
    """ part one """

    pattern = r"mul\((\d+,\d+)\)"
    matches = re.findall(pattern, memory)


    sum = 0 
    for match in matches:
        a, b = match.split(',')
        sum += int(a) * int(b)

    return sum


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    memory = ''
    for line in sys.stdin:
        memory += line.replace('\n', '')

    print('part_one', part_one(memory))

    print('part_two', part_two(memory))


main()
