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

    def run(self):
        computer(self.program, self)

    def send(self, command:str):
        for c in command:
            self.inputs.append(ord(c))
        self.inputs.append(10)

    def get_output(self):
        return ''.join(chr(c) for c in self.outputs[:-1])

    def get_last_output(self):
        return self.outputs[-1]

def part_one(program:list[int]):
    """Solution to part one."""
    droid = SpringDroid(program)
    droid.send('NOT A T')
    droid.send('NOT T T')
    droid.send('AND B T')
    droid.send('AND C T')
    droid.send('NOT T J')
    droid.send('AND D J')
    droid.send('WALK')
    droid.run()
    print(droid.get_output())
    return droid.get_last_output()


def part_two(program:list[int]):
    """Solution to part two."""
    droid = SpringDroid(program)
    droid.send('NOT A T')
    droid.send('NOT T T')
    droid.send('AND B T')
    droid.send('AND C T')
    droid.send('NOT T J')
    droid.send('AND D J')
    droid.send('NOT E T')
    droid.send('NOT T T')
    droid.send('OR H T')
    droid.send('AND T J')
    droid.send('RUN')
    droid.run()
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
