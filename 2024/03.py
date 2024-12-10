""" Day 3: Mull It over """
import sys
import re

PATTERN = r'mul\((\d+),(\d+)\)'

Mul = tuple[str, str]

def mul(mul_instruction:Mul)->int:
    """Apply the mul instruction."""
    return int(mul_instruction[0]) * int(mul_instruction[1])

def part_one(memory:str)->int:
    """Add up all uncorrupted mul instructions."""
    instructions = re.findall(PATTERN, memory)
    return sum(mul(m) for m in instructions)

def part_two(memory:str)->int:
    """Add up all enabled mull instructions."""
    enabled_sections = (do.split("don't()")[0] for do in memory.split('do()'))
    instructions = (m for enabled in enabled_sections for m in re.findall(PATTERN, enabled))
    return sum(mul(m) for m in instructions)

def main():
    """ main """
    memory = ''
    for line in sys.stdin:
        memory += line.replace('\n', '')

    print('part_one', part_one(memory))

    print('part_two', part_two(memory))

main()
