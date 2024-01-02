# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque

def part_one(player1, player2):
    """ part one """
    while player1 and player2:
        c1 = player1.popleft()
        c2 = player2.popleft()

        if c1 < c2:
            player2.append(c2)
            player2.append(c1)
        else:
            player1.append(c1)
            player1.append(c2)

    deck = reversed(player1 or player2)

    return sum((i+1)*c for i,c in enumerate(deck))

def part_two(player1, player2):
    """ part two """
    return 'todo'

def main():
    """ main """
    player1 = deque()
    player2 = deque()
    p2 = True
    for line in sys.stdin:
        line = line.replace('\n', '')
        if not line:
            continue
        if 'Player' in line:
            p2 = not p2
            continue

        if p2:
            player2.append(int(line))
        else:
            player1.append(int(line))




    print('part_one', part_one(player1, player2))

    print('part_two', part_two(player1, player2))


main()
