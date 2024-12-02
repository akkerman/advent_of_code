# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Set, Tuple


def get_most_blocks_idx(memory_banks: List[int]) -> int:
    most_blocks = sorted(memory_banks, reverse=True)[0]
    return memory_banks.index(most_blocks)

def redistribute(memory_banks: List[int], idx: int) -> None:
    blocks = memory_banks[idx]
    memory_banks[idx] = 0
    for _ in range(blocks):
        idx = (idx + 1) % len(memory_banks)
        memory_banks[idx] += 1

def part_one(memory_banks: List[int]) -> int:
    """ part one """
    seen: Set[Tuple[int, ...]] = set()

    while True:
        idx = get_most_blocks_idx(memory_banks)
        redistribute(memory_banks, idx)
        if tuple(memory_banks) in seen:
            return len(seen) + 1
        seen.add(tuple(memory_banks))



def part_two(memory_banks: List[int]) -> int:
    """ part two """
    # this works because part_one modifies memory_banks
    orig = tuple(memory_banks)
    cycles = 0
    while True:
        idx = get_most_blocks_idx(memory_banks)
        redistribute(memory_banks, idx)
        if tuple(memory_banks) == orig:
            return cycles + 1
        cycles += 1

def main():
    """ main """
    memory_banks: List[int] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        memory_banks = [int(b) for b in line.split('\t')]
        break
        
    print('part_one', part_one(memory_banks))

    print('part_two', part_two(memory_banks))


main()
