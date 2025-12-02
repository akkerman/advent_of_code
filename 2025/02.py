"""2025 Day 2: Gift Shop"""
import fileinput
import re
from utils import chunk_list

def is_invalid_id(id: int) -> bool:
    id_str = str(id)
    mid = len(id_str) // 2
    return id_str[:mid] == id_str[mid:]

def part_one(ranges:list[tuple[int,int]]):
    """Solution to part one."""
    total = 0
    for x,y in ranges:
        for id in range(x, y+1):
            if is_invalid_id(id):
                total += id
    return total

def is_invalid_id2(id: int) -> bool:
    id_str = str(id)
    for i in range(1, len(id_str)):
        parts: list[list[str]] = chunk_list(id_str, i)
        if all(part == parts[0] for part in parts):
            return True
    return False

def part_two(ranges:list[tuple[int,int]]):
    """Solution to part two."""
    total = 0
    for x,y in ranges:
        for id in range(x, y+1):
            if is_invalid_id2(id):
                total += id
    return total


def main():
    """Parse input file, pass to puzzle solvers."""
    line: str = next(fileinput.input()).strip()
    ranges: list[tuple[int,int]] = [(int(x),int(y)) for x,y in re.findall(r'(\d+)-(\d+)', line)]

    print('part_one', part_one(ranges))

    print('part_two', part_two(ranges))


if __name__ == '__main__':
    main()
