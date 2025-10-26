"""Day 15: Dueling Generators."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer


factor_a = 16807
factor_b = 48271

def next_value(previous:int, factor:int):
    """Generate the next value in the sequence."""
    modulus = 2147483647
    return (previous * factor) % modulus

def part_one(gen_a:int, gen_b:int):
    """Solution to part one."""
    curr_a: int = gen_a
    curr_b: int = gen_b
    

    count = 0

    for _ in range(40_000_000):
        curr_a = next_value(curr_a, factor_a) 
        curr_b = next_value(curr_b, factor_b)

        if (curr_a & 0xFFFF) == (curr_b & 0xFFFF):
            count += 1

    return count


def part_two(gen_a:int, gen_b:int):
    """Solution to part two."""
    print(gen_a, gen_b)
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    generators: list[int] = []
    for line in fileinput.input():
        line = line.strip()

        # match the numer at the end of the line
        matched =  re.search(r'(\d+)$', line)
        assert matched is not None
        start_value = int(matched.group(1))
        
        generators.append(start_value)

    print('part_one', part_one(*generators))

    print('part_two', part_two(*generators))


if __name__ == '__main__':
    main()
