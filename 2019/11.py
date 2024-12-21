"""Day 11: Space Police."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]
Dir = tuple[int, int]

MOVE = 0
PAINT = 1
LEFT = 0
RIGHT = 1
BLACK = 0
WHITE = 1
class Robot:
    def __init__(self):
       self.grid: dict[Coord, int] = defaultdict(lambda: BLACK)
       self.direction: Dir = (-1, 0)
       self.position: Coord = (0, 0)
       self.state = PAINT
       self.painted: set[Coord] = set()

    def instruct(self, value:int) -> None:
        if self.state == PAINT:
            self.grid[self.position] = value
            self.painted.add(self.position)
        if self.state == MOVE:
            dr, dc = self.direction
            if value == RIGHT:
                self.direction = (dc, -dr)
            if value == LEFT:
                self.direction = (-dc, dr)

            r, c = self.position
            dr, dc = self.direction
            self.position = (r+dr, c+dc)

        self.state^=1

    def see(self) -> int:
        return self.grid[self.position]


def computer(program: defaultdict[int,int], robot:Robot):
    """Computer."""
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    ADJUST_RELATIVE_BASE = 9
    HALT = 99

    MODE_POSITION = 0
    MODE_IMMEDIATE = 1
    MODE_RELATIVE = 2

    modes = [0,0,0,0]

    relative_base = 0

    def write(instruction_pointer:int, parameter:int, value:int):
        # parameters that an instruction writes to wil never be in immediate mode
        if modes[parameter] == MODE_POSITION:
            program[program[instruction_pointer+parameter]] = value
        elif modes[parameter] == MODE_RELATIVE:
            program[program[instruction_pointer+parameter] + relative_base] = value
        else:
            raise ValueError('Invalid mode for writing {}'.format(modes[parameter]))


    def read(instruction_pointer:int, parameter:int):
        if modes[parameter] == MODE_POSITION:
            return program[program[instruction_pointer + parameter]]
        elif modes[parameter] == MODE_IMMEDIATE:
            return program[instruction_pointer + parameter]
        elif modes[parameter] == MODE_RELATIVE:
            return program[program[instruction_pointer + parameter] + relative_base]
        else:
            raise ValueError('Invalid mode for reading {}'.format(modes[parameter]))

    i = 0
    while i < len(program):
        p3, p2, p1, o1, o2 = list(str(program[i]).zfill(5))
        modes = [0, int(p1), int(p2), int(p3)]
        op = int(o1 + o2)
        if op == HALT:
            break
        elif op == ADD:
            write(i, 3, read(i, 1) + read(i, 2))
            i+=4
        elif op == MULTIPLY:
            write(i, 3, read(i, 1) * read(i, 2))
            i+=4
        elif op == INPUT:
            write(i, 1, robot.see())
            i+=2
        elif op == OUTPUT:
            robot.instruct(read(i, 1))
            i+=2
        elif op == JUMP_IF_TRUE:
            if read(i, 1) != 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == JUMP_IF_FALSE:
            if read(i, 1) == 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == LESS_THAN:
            write(i, 3, 1 if read(i, 1) < read(i, 2) else 0)
            i+=4
        elif op == EQUALS:
            write(i, 3, 1 if read(i, 1) == read(i, 2) else 0)
            i+=4
        elif op == ADJUST_RELATIVE_BASE:
            relative_base += read(i, 1)
            i+=2
        else:
            raise ValueError('Unknown opcode {}'.format(op))

    return len(robot.painted)

def part_one(program:list[int]):
    """Solution to part one."""
    return computer(defaultdict(int, enumerate(program)), Robot())


def part_two(program:list[int]):
    """Solution to part two."""

    robot = Robot()
    robot.grid[(0,0)] = WHITE
    computer(defaultdict(int, enumerate(program)), robot)

    min_row = min(( r for r, _ in robot.painted ))
    max_row = max(( r for r, _ in robot.painted ))
    min_col = min(( c for _, c in robot.painted ))
    max_col = max(( c for _, c in robot.painted ))

    for r in range(min_row, max_row+1):
        for c in range(min_col, max_col+1):
            print('█' if robot.grid[(r,c)] == WHITE else ' ', end='')
        print()


def main():
    """Parse input file, pass to puzzle solvers."""
    program = []
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))
        break

    print('part_one', part_one(program.copy()))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
