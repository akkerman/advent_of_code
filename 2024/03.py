""" Day 3: Mull It over """
import sys
import re

pattern = r'mul\((\d+),(\d+)\)'

def mul(matches:list[str])->int:
    return sum(int(a) * int(b) for a,b in matches)

def part_one(memory:str)->int:
    """ part one """
    return mul(re.findall(pattern, memory))


def part_two(memory:str)->int:
    """ part two """
    sub_matches = (do.split("don't()")[0] for do in memory.split('do()'))
    matches = [m for sub in sub_matches for m in re.findall(pattern, sub)]
    return mul(matches)


def main():
    """ main """
    memory = ''
    for line in sys.stdin:
        memory += line.replace('\n', '')

    print('part_one', part_one(memory))

    print('part_two', part_two(memory))


main()
