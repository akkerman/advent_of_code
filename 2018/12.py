"""2018 Day 12: Subterranean Sustainability"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer


def generation(plants: dict[int, str], spread_notes: dict[str, str]) -> dict[int, str]:
    """Generate next generation of plants based on spread notes."""
    new_plants: dict[int, str] = {}

    min_index = min(plants.keys())
    max_index = max(plants.keys())

    for i in range(min_index - 2, max_index + 3):
        pattern = ''.join(
            plants.get(j, '.') for j in range(i - 2, i + 3)
        )
        new_plants[i] = spread_notes.get(pattern, '.')
    return new_plants

def part_one(spread_notes: list[tuple[str, str]], initial_state: str):
    """Solution to part one."""
    plants: dict[int, str] = {
            i: pot for i, pot in enumerate(initial_state)
            }

    for _ in range(20):
        plants = generation(plants, dict(spread_notes))

    return sum(i for i, pot in plants.items() if pot == '#')


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    initial_state: str = ''
    spread_notes: list[tuple[str, str]] = []

    for line in fileinput.input():
        if 'initial state' in line:
            initial_state = line.strip().split(': ')[1]
            continue
        if '=>' in line:
            f, t = line.strip().split(' => ')
            spread_notes.append((f, t))
            continue

    print('part_one', part_one(spread_notes, initial_state))

    print('part_two', part_two(spread_notes))


if __name__ == '__main__':
    main()
