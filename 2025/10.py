"""2025 Day 10: Factory"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Lights = list[bool]
Button = list[int]
Joltage = list[int]
Machine = tuple[Lights, list[Button], Joltage]

def min_presses(machine: Machine) -> int:
    """Simulate machine switching and return number of lights on."""
    end, buttons, _ = machine

    queue: deque[tuple[int, Lights]] = deque()
    queue.append((0, [False] * len(machine[0])))
    while queue:
        presses, lights = queue.popleft()
        if lights == end:
            return presses
        
        for button in buttons:
            new_lights = lights[:]
            for idx in button:
                new_lights[idx] = not new_lights[idx]
            queue.append((presses + 1, new_lights))

    assert False, "Shouldn't reach here"



def part_one(machines: list[Machine]):
    """Solution to part one."""
    return sum(min_presses(m) for m in machines)

def part_two(lines):
    """Solution to part two."""
    return 'todo'


def parse(button: str):
    """Parse button string into tuple."""
    res = button.strip('()')
    return [int(x) for x in res.split(',')]

def main():
    """Parse input file, pass to puzzle solvers."""
    machines = list[Machine]()
    for line in fileinput.input():
        parts  = line.strip().split(' ')
        lights = [s == '#' for s in parts[0].strip(r'\[\]')]
        buttons = [parse(button) for button in parts[1:-1]]
        joltage = [int(x) for x in parts[2].strip('()').split(',')]

        machines.append((lights, buttons, joltage))


    print('part_one', part_one(machines))

    print('part_two', part_two(machines))


if __name__ == '__main__':
    main()
