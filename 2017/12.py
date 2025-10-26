"""Day 12: Digital Plumber."""
import fileinput
from collections import deque, defaultdict
from typing import Dict

def part_one(door_dict: Dict[int, list[int]]):
    """Solution to part one."""

    return len(determine_group(door_dict))

def determine_group(door_dict: Dict[int, list[int]], start: int=0) -> set[int]:
    group:set[int] = set()
    group.add(start)
    queue:deque[int] = deque()
    queue.append(start)
    while queue:
        current = queue.popleft()
        for door in door_dict[current]:
            if door not in group:
                group.add(door)
                queue.append(door)

    return group


def part_two(door_dict: Dict[int, list[int]]):
    """Solution to part two."""
    groups: list[set[int]] = []
    
    all_doors = set(door_dict.keys())

    current_door = 0

    while True:
        groups.append(determine_group(door_dict, current_door))
        
        remaining_doors = all_doors - {elem for s in groups for elem in s}

        if not remaining_doors:
            return len(groups)

        current_door = min(remaining_doors)


def main():
    """Parse input file, pass to puzzle solvers."""
    door_dict: Dict[int, list[int]] = defaultdict(list[int])
    for line in fileinput.input():
        line = line.strip()
        fd, doors =  line.split(' <-> ')
        doors = list(map(int, doors.split(', ')))
        
        
        door_dict[int(fd)].extend(doors)

    print('part_one', part_one(door_dict))

    print('part_two', part_two(door_dict))


if __name__ == '__main__':
    main()
