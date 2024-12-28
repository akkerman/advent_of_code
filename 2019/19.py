"""Day 19: Tractor Beam."""
import fileinput
from collections import  defaultdict
from intcode import computer, IO

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
    return sum(drone.out)


def part_two(program: list[int]) -> int:
    """Solution to part two."""
    drone = Drone()
    def check(x:int, y:int):
        drone.instructions = [x, y]
        computer(defaultdict(int, enumerate(program)), drone)
        return drone.out[-1] == 1



    x = 0
    y = 100
    while True:
        while not check(x,y):
            x += 1

        if check(x+99, y-99):
            return x*10000 + y-99

        y += 1


        
    
    

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
