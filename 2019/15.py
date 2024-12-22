"""Day 15: Oxygen System."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
import random


NORTH = 1
SOUTH = 2
WEST = 3
EAST = 4


UNEXPLORED = -1
WALL = 0
MOVED = 1
FOUND = 2

Coord = tuple[int,int]

wind2dir = {
    NORTH: (0,1),
    SOUTH: (0,-1),
    WEST: (-1,0),
    EAST: (1,0)
}

class Droid:
    def __init__(self):
        self.current_status = MOVED
        self.current_dir = NORTH
        self.current_pos = (0,0)
        self.maze: dict[Coord, int] = defaultdict(lambda: UNEXPLORED)
        self.oxygen_pos: Coord = (0,0)
        self.steps = 0

    def move(self) -> int:
        if self.oxygen_pos != (0,0):
            return 0

        for i in range(0,5):
            dir = (self.current_dir + i) % 4
            dir = dir if dir != 0 else 4
            if self.maze[self.next_pos(dir)] == UNEXPLORED:
                self.current_dir = dir
                return dir

        for i in range(0,5):
            dir = (self.current_dir + i) % 4
            dir = dir if dir != 0 else 4
            if self.maze[self.next_pos(dir)] != WALL:
                self.current_dir = dir
                return dir

        return 0

    def status(self, value:int) -> None:
        self.current_status = value
        if value == WALL:
            self.maze[self.next_pos()] = WALL
            return

        self.maze[self.current_pos] = value
        self.current_pos = self.next_pos()

        if value == FOUND:
            print('Found oxygen system at', self.current_pos, 'in', self.steps, 'steps')
            self.oxygen_pos = self.current_pos

    def next_pos(self, dir:int=0) -> Coord:
        x,y = self.current_pos
        dx,dy = wind2dir[dir] if dir else wind2dir[self.current_dir]
        return (x+dx, y+dy)


def computer(program: defaultdict[int,int], droid:Droid):
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

    output:list[int] = []

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
            write(i, 1, droid.move())
            i+=2
        elif op == OUTPUT:
            # output.append(read(i, 1))
            droid.status(read(i, 1))
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

    return output


def part_one(program: list[int]) -> int:
    """Solution to part one."""
    droid = Droid()
    computer(defaultdict(int, enumerate(program)), droid)
    print(droid.maze)

    q = [(0,0,0)]
    visited:set[Coord] = set()
    while q:
        steps, x, y = heapq.heappop(q)
        if (x,y) == droid.oxygen_pos:
            return steps
        if (x,y) in visited: continue
        visited.add((x,y))
        for dx,dy in wind2dir.values():
            nx,ny = x+dx, y+dy
            if droid.maze[(nx,ny)] == WALL:
                continue
            heapq.heappush(q, (steps+1, nx, ny))

    return -1


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    program: list[int] = []
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
