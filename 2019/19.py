"""Day 19: Tractor Beam."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from intcode import computer, IO

class Drone(IO):
    def __init__(self):
        self.instructions = list[int]()
        self.out = list[int]()
        for i in range(50):
            for j in range(50):
                self.instructions.append(i)
                self.instructions.append(j)

    def input(self):
        if self.instructions:
            return self.instructions.pop(0)
        return -1

    def output(self, value:int):
        self.out.append(value)

def part_one(program: list[int]) -> int:
    """Solution to part one."""
    drone = Drone()
    for _ in range(50*50):
        computer(defaultdict(int, enumerate(program)), drone)
    return sum(drone.out)


def part_two(program: list[int]) -> int:
    """Solution to part two."""
    return -1


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))
        


    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
