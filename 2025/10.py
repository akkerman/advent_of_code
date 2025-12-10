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

@perf_timer
def min_presses_light(machine: Machine) -> int:
    """Simulate machine switching and return number of steps to get the desired combination of lights on."""
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

@perf_timer
def min_presses_joltage(machine: Machine) -> int:
    """Simulate machine switching and return number pressesn to match the joltage requirement."""
    _, buttons, required_joltage = machine

    def check(current_joltage: list[int]) -> bool:
        return all(c <= r for c, r in zip(current_joltage, required_joltage))

    queue: deque[tuple[int, Joltage]] = deque()
    queue.append((0, [0] * len(required_joltage)))

    while queue:
        presses, joltage = queue.popleft()
        if joltage == required_joltage:
            return presses
        
        for button in buttons:
            new_joltage = list(joltage)
            for idx in button:
                new_joltage[idx] += 1
            if check(new_joltage):
                queue.append((presses + 1, new_joltage))

    assert False, "Shouldn't reach here"


@perf_timer
def part_one(machines: list[Machine]):
    """Solution to part one."""
    return sum(min_presses_light(m) for m in machines)

def part_two(machines: list[Machine]):
    """Solution to part two."""
    return sum(min_presses_joltage(m) for m in machines)

    


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
        joltage = [int(x) for x in parts[-1].strip('{}').split(',')]

        machines.append((lights, buttons, joltage))


    # print('part_one', part_one(machines))

    print('part_two', part_two(machines))


if __name__ == '__main__':
    main()
