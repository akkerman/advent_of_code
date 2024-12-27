"""Day 19: Tractor Beam."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from intcode import computer, IO
from itertools import count

class Drone(IO):
    def __init__(self):
        self.instructions = list[int]()
        self.out = list[int]()
        self.coords = set[tuple[int, int]]()
        for y in range(50):
            for x in range(50):
                self.instructions.append(x)
                self.instructions.append(y)

    def input(self):
        if self.instructions:
            return self.instructions.pop(0)
        return -1

    def output(self, value:int):
        self.out.append(value)

    def reconstruct_coords(self):
        for y in range(50):
            for x in range(50):
                if self.out[y*50 + x] == 1:
                    self.coords.add((y, x))

    def print_map(self):
        self.reconstruct_coords()
        for y in range(50):
            for x in range(50):
                if (y, x) in self.coords:
                    print('#', end='')
                else:
                    print('.', end='')
            print()


def part_one(program: list[int]) -> int:
    """Solution to part one."""
    drone = Drone()
    for _ in range(50*50):
        computer(defaultdict(int, enumerate(program)), drone)
    drone.print_map()
    return sum(drone.out)


def part_two(program: list[int]) -> int:
    """Solution to part two."""
    drone = Drone()
    def check(x:int, y:int):
        print('checking', x, y)
        drone.instructions = [x, y]
        computer(defaultdict(int, enumerate(program)), drone)
        return drone.out[0] == 1


    left = 0
    for y in count(20):
        for x in count(left):
            if not check(x,y): continue
            print(x,y)
            left = x
            if not check(x+100, y-100): break
            return x*10000 + y-100
    
    

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
