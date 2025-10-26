"""Day 12: Digital Plumber."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from typing import Dict
from utils import perf_timer

def part_one(door_dict: Dict[str, list[int]]):
    """Solution to part one."""
    group:set[int] = set()
    group.add(0)
    queue:deque[int] = deque()
    queue.append(0)
    while queue:
        current = queue.popleft()
        for door in door_dict[str(current)]:
            if door not in group:
                group.add(door)
                queue.append(door)

    return len(group)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    door_dict: Dict[str, list[int]] = defaultdict(list[int])
    for line in fileinput.input():
        line = line.strip()
        fd, doors =  line.split(' <-> ')
        doors = list(map(int, doors.split(', ')))
        
        
        door_dict[fd].extend(doors)

    print('part_one', part_one(door_dict))

    print('part_two', part_two(door_dict))


if __name__ == '__main__':
    main()
