# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
from collections import defaultdict, deque
import sys


def part_one(lines):
    """ part one """
    contained_in = defaultdict(list)
    for bag, contains in lines:
        for _, contained in contains:
            contained_in[contained].append(bag)

    # BFS
    q = deque(['shiny gold'])
    visited = set()
    while q:
        bag = q.popleft()
        visited.add(bag)
        for container in contained_in[bag]:
            q.append(container)

    return len(visited) - 1  # 'shiny gold' does not contain itself


def part_two(lines):
    """ part two """
    containers = defaultdict(list)
    for bag, contains in lines:
        containers[bag] = contains

    total = 0
    q = deque([(1, 'shiny gold')])

    while q:
        n, bag = q.popleft()
        total += n
        for m, contained in containers[bag]:
            q.append((n*m, contained))

    return total - 1  # 'shiny gold' does not contain itself


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = line.replace(' bags.', '').replace(' bags', '')
        line = line.replace(' bag.', '').replace(' bag', '')
        line = line.replace('no other', '0 other')

        bag, contains = line.split(' contain ')
        contains = [(int(s[0]), s[2:]) for s in contains.split(', ')]

        lines.append((bag, contains))

    print('part_one', part_one(lines))
    print('part_two', part_two(lines))


main()
