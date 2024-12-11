"""Day 11: Plutonian Pebbles."""
import sys

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


def part_one(stones: list[int], blinks: int = 25) -> int:
    """Solution to part one."""
    new_stones = stones
    for _ in range(blinks):
        new_stones = blink(new_stones)
        # print(f'After {_ + 1} blinks:\n{new_stones}\n')
    return len(new_stones)


def part_two(stones: list[int]) -> int:
    """Solution to part two."""
    return part_one(stones, 75)


def main():
    """Parse input file, pass to puzzle solvers."""
    stones: list[int] = []
    for line in sys.stdin:
        line = line.strip()
        stones.extend(list(map(int, line.split(' '))))
        

    print('part_one', part_one(stones))

    print('part_two', part_two(stones))


main()
