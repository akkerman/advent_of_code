"""Day 22: Slam Shuffle."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def deal_with_increment(deck: list[int], inc: int):
    """Deal with increment."""
    new_deck = deck.copy()
    new_deck = [0] * len(deck)
    for i, card in enumerate(deck):
        new_deck[(i * inc) % len(deck)] = card
    return new_deck

NUMBER = re.compile(r'-?\d+')

def shuffle(deck:list[int], instructions: list[str]):
    """Shuffle deck according to instructions."""

    new_deck = deck.copy()

    for instruction in instructions:
        if 'deal into new stack' in instruction:
            new_deck = new_deck[::-1]
            continue

        num = int(re.findall(NUMBER, instruction)[0])

        if 'deal with increment' in instruction:
            new_deck = deal_with_increment(new_deck, num)
            continue

        if 'cut' in instruction:
            new_deck = new_deck[num:] + new_deck[:num]
            continue

    return new_deck


# too high: 8611
def part_one(lines: list[str]) -> int:
    """Solution to part one."""
    new_deck = shuffle(list(range(10007)), lines)
    return new_deck.index(2019)


def part_two(lines):
    """Solution to part two."""
    return -1


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        line = line.strip()
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()

def test_shuffle_1():
    deck = list(range(10))
    instructions = [
        'deal with increment 7',
        'deal into new stack',
        'deal into new stack'
    ]
    assert shuffle(deck, instructions) == [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]

def test_shuffle_2():
    deck = list(range(10))
    instructions = [
        'cut 6',
        'deal with increment 7',
        'deal into new stack'
    ]
    assert shuffle(deck, instructions) == [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]

def test_shuffle_3():
    deck = list(range(10))
    instructions = [
        'deal with increment 7',
        'deal with increment 9',
        'cut -2'
    ]
    assert shuffle(deck, instructions) == [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]

def test_shuffle_4():
    deck = list(range(10))
    instructions = [
        'deal into new stack',
        'cut -2',
        'deal with increment 7',
        'cut 8',
        'cut -4',
        'deal with increment 7',
        'cut 3',
        'deal with increment 9',
        'deal with increment 3',
        'cut -1'
     ]
    assert shuffle(deck, instructions) == [9,2,5,8,1,4,7,0,3,6]
