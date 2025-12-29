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

def generation2(plants: str, spread_notes: dict[str, str]) -> str:
    """Generate next generation of plants based on spread notes."""
    new_plants: list[str] = []

    padded_plants = '....' + plants + '....'

    for i in range(2, len(padded_plants) - 2):
        pattern = padded_plants[i - 2:i + 3]
        new_plants.append(spread_notes.get(pattern, '.'))
    return ''.join(new_plants)

@perf_timer
def part_one(spread_notes: list[tuple[str, str]], initial_state: str):
    """Solution to part one."""
    plants: dict[int, str] = {
            i: pot for i, pot in enumerate(initial_state)
            }

    for _ in range(20):
        plants = generation(plants, dict(spread_notes))

    return sum(i for i, pot in plants.items() if pot == '#')


@perf_timer
def part_two(spread_notes: list[tuple[str, str]], initial_state: str) -> int:
    """Solution to part two."""
    plants: str = initial_state
    seen: dict[int, int] = {}
    for i in range(50000000000):
        plants = generation2(plants, dict(spread_notes))
        offset = (i + 1) * 2
        total = sum(i - offset for i, pot in enumerate(plants) if pot == '#')
        if total in seen:
            cycle_length = i - seen[total]
            remaining_cycles = (50000000000 - i - 1) // cycle_length
            total += remaining_cycles * cycle_length
            return total
        seen[total] = i


    assert False, "Shouldn't reach here"






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

    print('part_two', part_two(spread_notes, initial_state))


if __name__ == '__main__':
    main()
