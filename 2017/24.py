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

def part_one(components: list[Component]):
    """Solution to part one."""
    max_strength = 0
    queue: list[tuple[Strength, Port, Used]] = [(0, 0, set())]

    while queue:
        strength, port, used = heapq.heappop(queue)
        max_strength = max(max_strength, strength)

        for component in components:
            if component in used or port not in component:
                continue
            f,t = component
            next_port = t if f == port else f 
            next_strength = strength + f + t
            next_used = used | {component}
            heapq.heappush(queue, (next_strength, next_port, next_used))

    return max_strength






def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    components = list[Component]()
    for line in fileinput.input():
        f,t = line.strip().split('/')
        f,t = int(f), int(t)
        components.append((f,t))

    print('part_one', part_one(components))

    print('part_two', part_two(components))


if __name__ == '__main__':
    main()
