"""Day 11: Plutonian Pebbles."""
import sys
from functools import cache

from utils import perf_timer

def split_stone(stone: int) -> list[int]:
    """Split a stone."""
    stone_str = str(stone)
    half = len(stone_str) // 2
    return [int(stone_str[:half]), int(stone_str[half:])]

def blink(stones: list[int]) -> list[int]:
    """Blink at the stones."""
    new_stones: list[int] = []
    for stone in stones:
        if stone == 0:
            new_stones.append(1)
        elif len(str(stone)) % 2 == 0:
            new_stones.extend(split_stone(stone))
        else:
            new_stones.append(stone * 2024)
    return new_stones


@perf_timer
def part_one(stones: list[int]) -> int:
    """Number of stones after 25 blinks."""
    count = 0
    for st in stones:
        new_stones = [st]
        for _ in range(25):
            new_stones = blink(new_stones)
        count+=len(new_stones)
    return count


@cache
def solve(stone: int, blinks: int) -> int:
    """Calculate the number of stones, from one stone after given blinks."""
    if blinks == 0:
        return 1
    if len(str(stone)) % 2 == 0:
        l, r = split_stone(stone)
        return solve(l, blinks - 1) + solve(r, blinks - 1)
    if stone == 0:
        return solve(1, blinks - 1)
    return solve(stone * 2024, blinks - 1)

@perf_timer
def part_two(stones: list[int]) -> int:
    """Number of stones after 75 blinks."""
    return sum(solve(stone, 75) for stone in stones)

@perf_timer
def test(stones: list[int]) -> int:
    return sum(solve(stone, 25) for stone in stones)

def main():
    """Parse input file, pass to puzzle solvers."""
    stones: list[int] = []
    for line in sys.stdin:
        line = line.strip()
        stones.extend(list(map(int, line.split(' '))))

    print('part_one', part_one(stones))
    print('part_two', part_two(stones))


main()
