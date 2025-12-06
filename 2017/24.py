"""2017 Day 24: Electromagnetic Moat"""
import fileinput
from collections import deque, defaultdict

Port = int
Component = tuple[Port, Port]
Strength = int
Used = set[Component]

def part_one(components: dict[Port,set[Component]]):
    """Solution to part one."""
    max_strength = 0
    queue: deque[tuple[Strength, Port, Used]] = deque([(0, 0, set())])

    while queue:
        strength, port, used = queue.popleft()
        max_strength = max(max_strength, strength)

        for component in (components[port] - used):
            left, right = component

            s = strength + left + right
            p = right if left == port else left  # order doesn't matter
            u = used | {component}

            queue.append((s, p, u))

    return max_strength


def part_two(components: dict[Port,set[Component]]):
    """Solution to part one."""
    max_strength = 0
    max_length = 0
    queue: deque[tuple[Strength, Port, Used]] = deque([(0, 0, set())])

    while queue:
        strength, port, used = queue.popleft()

        length = len(used)
        if length > max_length:
            max_length = length
            max_strength = strength
        elif length == max_length:
            max_strength = max(max_strength, strength)

        for component in (components[port] - used):
            left, right = component

            s = strength + left + right
            p = right if left == port else left  # order doesn't matter
            u = used | {component}

            queue.append((s, p, u))

    return max_strength


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
