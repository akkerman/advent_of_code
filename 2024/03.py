# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from typing import List


pattern = r'mul\((\d+,\d+)\)'

def mul(matches:List[str])->int:
    sum = 0 
    for match in matches:
        a, b = match.split(',')
        sum += int(a) * int(b)
    return sum

def part_one(memory:str)->int:
    """ part one """
    matches = re.findall(pattern, memory)
    return mul(matches)


def part_two(memory:str)->int:
    """ part two """
    dos = memory.split('do()')
    matches: List[str] = []
    for do in dos:
        stilldo = do.split("don't()")[0]
        matches += re.findall(pattern, stilldo)
    return mul(matches)


def main():
    """ main """
    memory = ''
    for line in sys.stdin:
        memory += line.replace('\n', '')

    print('part_one', part_one(memory))

    print('part_two', part_two(memory))


main()
