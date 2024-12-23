"""Day 13: Care Package."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
import os

import time


Coord = tuple[int, int]




tiles = [' ', '#',  '█', '_', 'O']

BALL = 4
PADDLE = 3


class Arcade:
    def __init__(self):
        self.screen: dict[Coord, str] = defaultdict(lambda: ' ')
        self.params: list[int] = []
        self.score = 0
        self.frame = 0
        self.ball = (0,0)
        self.paddle = (0,0)
        self.playing = False

    def draw(self, value:int):
        self.params.append(value)
        if len(self.params) == 3:
            x, y, tile = self.params
            if x == -1 and y == 0:
                self.score = tile
                self.params = []
                return
            if tile == BALL:
                self.ball = (x,y)
            if tile == PADDLE:
                self.paddle = (x,y)
            
            self.screen[(x,y)] = tiles[tile]
            self.params = []
            # if self.playing:
            #     self.print_screen()
            #     time.sleep(0.1)

    def print_screen(self):
        print("\033[H\033[J", end="")
        self.frame+=1
        print('Score:', self.score, 'Frame:', self.frame)
        for y in range(0, 23):
            for x in range(0, 40):
                print(self.screen[(x,y)], end='')
            print()
        print()

    def joystick(self):
        self.playing = True
        if self.ball[0] < self.paddle[0]:
            return -1
        elif self.ball[0] > self.paddle[0]:
            return 1
        else:
            return 0
        

def computer(program: defaultdict[int,int], arcade: Arcade):
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
            write(i, 1, arcade.joystick())
            i+=2
        elif op == OUTPUT:
            arcade.draw(read(i, 1))
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


def part_one(program: list[int]) -> int:
    """Solution to part one."""
    arcade = Arcade()


    computer(defaultdict(int, enumerate(program)), arcade)
    return Counter(arcade.screen.values())['█']


def part_two(program: list[int]) -> int:
    """Solution to part two."""
    arcade = Arcade()

    program[0] = 2
    computer(defaultdict(int, enumerate(program)), arcade)
    return arcade.score


def main():
    """Parse input file, pass to puzzle solvers."""
    program: list[int] = []
    for line in fileinput.input():
        program = list(map(int, line.strip().split(',')))

    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
