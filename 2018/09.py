"""2018 Day 9: Marble Mania"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def marble_game(num_players: int, last_marble: int) -> int:
    """Play the marble game and return the winning score."""
    circle = deque[int]() 
    scores = [0] * num_players

    circle.append(0) # First, the marble numbered 0 is placed in the circle
    for marble in range(1, last_marble + 1):
        if marble % 23 == 0:
            player_id = marble % num_players

            # The current player keeps the marble they would have placed,
            # adding it to their score.
            scores[player_id] += marble

            # In addition, the marble 7 marbles counter-clockwise from
            # the current marble is removed from the circle and also added
            # to the current player's score. 
            circle.rotate(7)
            scores[player_id] += circle.pop()

            # The marble located immediately clockwise of the marble
            # that was removed becomes the new current marble.
            circle.rotate(-1)
            continue
    
        # Place the new marble between the marbles that are 
        # 1 and 2 marbles clockwise of the current marble. 
        circle.rotate(-1)
        circle.append(marble)

    return max(scores)

def part_one(num_players: int, last_marble: int):
    """Solution to part one."""
    return marble_game(num_players, last_marble)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


re_marble = re.compile(r'(\d+) players; last marble is worth (\d+) points')
def parse(line: str) -> tuple[int, int]:
    match = re_marble.match(line)
    assert match
    num_players = int(match[1])
    last_marble = int(match[2])
    return num_players, last_marble

def main():
    """Parse input file, pass to puzzle solvers."""
    first: str = next(fileinput.input()).strip()

    print('part_one', part_one(*parse(first)))

    print('part_two', part_two(first))


if __name__ == '__main__':
    main()

def test_1():
    line = '10 players; last marble is worth 1618 points'
    expect = 8317
    assert marble_game(*parse(line)) == expect

def test_2():
    line = '13 players; last marble is worth 7999 points'
    expect = 146373
    assert marble_game(*parse(line)) == expect

def test_3():
    line = '17 players; last marble is worth 1104 points'
    expect = 2764
    assert marble_game(*parse(line)) == expect

def test_4():
    line = '21 players; last marble is worth 6111 points'
    expect = 54718
    assert marble_game(*parse(line)) == expect

def test_5():
    line = '30 players; last marble is worth 5807 points'
    expect = 37305
    assert marble_game(*parse(line)) == expect

