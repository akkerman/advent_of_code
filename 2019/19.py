"""Day 19: Tractor Beam."""
import fileinput
from intcode import computer, IO
from utils import perf_timer

class Drone(IO):
    def __init__(self, program:list[int]):
        self.program = program
        self.instructions = list[int]()

    def input(self):
        if self.instructions:
            return self.instructions.pop(0)
        return -1

    def output(self, value:int):
        self.out = value

    def is_affected(self, x:int, y: int):
        """Check if this drone is affected by the beam at given coordinate"""
        self.instructions = [x,y]
        computer(self.program, self)
        return self.out == 1

    def total_affected(self, width: int):
        """Total points the beam affects this droids in a square area of given width"""
        total = 0
        for y in range(width):
            for x in range(width):
                if self.is_affected(x,y):
                    total += 1
        return total

    def find_square(self, width: int):
        """Find closest point of a square of given width that fits in the tractor beam"""
        x = 0
        y = width
        while True:
            while not self.is_affected(x,y):
                x += 1
            if self.is_affected(x+width-1, y-width+1):
                return x*10000 + y-width+1
            y += 1

@perf_timer
def part_one(program: list[int]) -> int:
    """Solution to part one."""
    return Drone(program).total_affected(50)

@perf_timer
def part_two(program: list[int]) -> int:
    """Solution to part two."""
    return Drone(program).find_square(100)

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
