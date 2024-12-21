"""Day 21: Keypad Conundrum."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
import itertools
numpad = [['7', '8', '9'], ['4', '5', '6'], ['1', '2', '3'], [' ', '0', 'A']] 
dirpad = [[' ', '^', 'A'], ['<', 'v', '>']]

def keypad_positions(keypad: list[list[str]]):
    """Return all possible positions on the keypad."""
    pos: dict[str, tuple[int, int]] = {}
    for r, row in enumerate(keypad):
        for c, key in enumerate(row):
            if key == ' ': continue
            pos[key] = (r, c)
    return pos


def shortest_sequences(keypad: list[list[str]]):
    """Return all possible shortes sequences from one button to another."""
    positions = keypad_positions(keypad)
    sequences: dict[tuple[str, str], list[str]] = defaultdict(list)
    for start in positions:
        for end in positions:
            if start == end: 
                sequences[(start, end)] = ['A']
                continue

            q = deque([(positions[start], "")])
            shortest_path = float('inf')
            while q:
                (r, c), path = q.popleft()
                if len(path) >= shortest_path: continue
                if (r, c) == positions[end]:
                    sequences[(start, end)].append(path + 'A')
                    shortest_path = len(path) + 1
                    continue

                for (nr, nc), dir in zip([(r+1,c), (r-1,c), (r,c+1), (r,c-1)], ['v', '^', '>', '<']):
                    if (nr, nc) not in positions.values(): continue
                    q.append(((nr, nc), path + dir))

    return sequences

def make_keypresses(keypad: list[list[str]]):
    shortest = shortest_sequences(keypad)
    def keypresses(code:str):
        all_shortest = [shortest[a] for a in zip('A'+code, code)]
        return [ "".join(t) for t in itertools.product(*all_shortest) ]
    return keypresses

numpad_keypresses = make_keypresses(numpad)
dirpad_keypresses = make_keypresses(dirpad)

def shortest_sequence(code: str):
    robot1 = numpad_keypresses(code)
    robot2 = [keys for keys1 in robot1 for keys in dirpad_keypresses(keys1)]
    min_length = min(map(len, robot2))
    robot2 = [s for s in robot2 if len(s) == min_length]
    robot3 = [keys for keys2 in robot2 for keys in dirpad_keypresses(keys2)]
    return min(map(len, robot3))

def shortest_sequence_v2(code: str, num_robots: int):
    robot = numpad_keypresses(code)

    for _ in range(num_robots-1):
        robot = [keys for keys1 in robot for keys in dirpad_keypresses(keys1)]
        min_length = min(map(len, robot))
        robot = [s for s in robot if len(s) == min_length]

    return min(map(len, robot))


@perf_timer
def part_one(codes: list[str]):
    """Solution to part one."""
    return sum(int(code[:-1]) * shortest_sequence(code) for code in codes)

@perf_timer
def part_two(codes: list[str]):
    """Solution to part two."""
    return sum(int(code[:-1]) * shortest_sequence_v2(code, 3) for code in codes)


def main():
    """Parse input file, pass to puzzle solvers."""
    codes: list[str] = []
    for line in fileinput.input():
        codes.append(line.strip())

    print('part_one', part_one(codes))

    print('part_two', part_two(codes))


if __name__ == '__main__':
    main()

def test_num_presses_029A():
    assert sorted(numpad_keypresses('029A')) == sorted(['<A^A>^^AvvvA','<A^A^>^AvvvA','<A^A^^>AvvvA'])

def test_dir_num_presses_029A():
    robot1 = numpad_keypresses('029A')
    robot2 = [keys for keys1 in robot1 for keys in dirpad_keypresses(keys1)]

    assert 'v<<A>>^A<A>AvA<^AA>A<vAAA>^A' in robot2

def test_dir_dir_num_presses_029A():
    robot1 = numpad_keypresses('029A')
    robot2 = [keys for keys1 in robot1 for keys in dirpad_keypresses(keys1)]
    robot3 = [keys for keys2 in robot2 for keys in dirpad_keypresses(keys2)]

    assert '<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A' in robot3


def test_shortest_sequence_029A():
    assert shortest_sequence('029A') == len('<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A')
def test_shortest_sequence_980A():
    assert shortest_sequence('980A') == len('<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A')
def test_shortest_sequence_179A():
    assert shortest_sequence('179A') == len('<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A')
def test_shortest_sequence_456A():
    assert shortest_sequence('456A') == len('<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A')
def test_shortest_sequence_379A():
    assert shortest_sequence('379A') == len('<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A')
