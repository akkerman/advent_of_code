"""Day 21: Springdroid Adventure."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from intcode import computer, IO

class SpringDroid(IO):
    def __init__(self, program:list[int]):
        self.program = program
        self.inputs = deque[int]()
        self.outputs = list[int]()

    def input(self):
        return self.inputs.popleft()

    def output(self, value:int):
        self.outputs.append(value)

    def start(self):
        computer(self.program, self)

    def instruct(self, command:str):
        for c in command:
            self.inputs.append(ord(c))
        self.inputs.append(10)
        return self

    def get_output(self):
        return ''.join(chr(c) for c in self.outputs[:-1])

    def get_last_output(self):
        return self.outputs[-1]

def part_one(program:list[int]):
    """Solution to part one."""
    droid = SpringDroid(program)
    droid.instruct('NOT A T')
    droid.instruct('NOT T T')
    droid.instruct('AND B T')
    droid.instruct('AND C T')
    droid.instruct('NOT T J')
    droid.instruct('AND D J')
    droid.instruct('WALK')
    droid.start()
    print(droid.get_output())
    return droid.get_last_output()


def part_two(program:list[int]):
    """Solution to part two."""
    droid = SpringDroid(program)
    droid.instruct('NOT A T')
    droid.instruct('NOT T T')
    droid.instruct('AND B T')
    droid.instruct('AND C T')
    droid.instruct('NOT T J')
    droid.instruct('AND D J')
    droid.instruct('NOT E T')
    droid.instruct('NOT T T')
    droid.instruct('OR H T')
    droid.instruct('AND T J')
    droid.instruct('RUN')
    droid.start()
    print(droid.get_output())
    return droid.get_last_output()


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
