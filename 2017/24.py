"""2017 Day 24: Electromagnetic Moat"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Port = int
Component = tuple[Port, Port]
Strength = int
Used = set[Component]

@perf_timer
def part_one(components: dict[Port,set[Component]]):
    """Solution to part one."""
    max_strength = 0
    queue: list[tuple[Strength, Port, Used]] = [(0, 0, set())]

    while queue:
        strength, port, used = heapq.heappop(queue)
        max_strength = max(max_strength, strength)

        for component in (components[port] - used):
            left, right = component

            s = strength + left + right
            p = right if left == port else left  # order doesn't matter
            u = used | {component}

            heapq.heappush(queue, (s, p, u))

    return max_strength


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    components: dict[Port, set[Component]] = defaultdict(set)
    for line in fileinput.input():
        left, right = line.strip().split('/')
        left, right = int(left), int(right)

        component = (left, right)
        components[left].add(component)
        components[right].add(component)

        

    print('part_one', part_one(components))

    print('part_two', part_two(components))


if __name__ == '__main__':
    main()
