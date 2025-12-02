"""2025 Day 2: Gift Shop"""
import fileinput
import re
from utils import chunk_list
from typing import Callable

def is_invalid_id_part1(id: int) -> bool:
    """ Check if the given id is invalid. It is invalid if a pattern is repeated twice."""
    id_str = str(id)
    mid = len(id_str) // 2
    return id_str[:mid] == id_str[mid:]

def is_invalid_id_part2(id: int) -> bool:
    """ Check if the given id is invalid. It is invalid if it is a repeating pattern."""
    id_str = str(id)
    mid = len(id_str) // 2 
    for i in range(1, mid + 1):
        parts: list[list[str]] = chunk_list(id_str, i)
        if all(part == parts[0] for part in parts):
            return True
    return False

def solve(ranges:list[tuple[int,int]], is_invalid_fn: Callable[[int],bool]) -> int:
    """Sum all invalid IDs in the given ranges."""
    return sum(id for x,y in ranges for id in range(x, y+1) if is_invalid_fn(id))

def main():
    """Parse input file, pass to puzzle solvers."""
    line: str = next(fileinput.input()).strip()
    ranges: list[tuple[int,int]] = [(int(x),int(y)) for x,y in re.findall(r'(\d+)-(\d+)', line)]

    print('part_one', solve(ranges, is_invalid_id_part1))
    print('part_two', solve(ranges, is_invalid_id_part2))


if __name__ == '__main__':
    main()
