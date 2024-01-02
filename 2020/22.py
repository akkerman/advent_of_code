# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque
from functools import cache

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


    @cache
    def game(tp1, tp2):
        player1 = deque(tp1)
        player2 = deque(tp2)
        states1 = set()
        states2 = set()

        round_nbr = 0
        while player1 and player2:
            round_nbr += 1
            tp1 = tuple(player1)
            tp2 = tuple(player2)
            if tp1 in states1 or tp2 in states2:
                return tuple(player1), ()

            states1.add(tp1)
            states2.add(tp2)

            c1 = player1.popleft()
            c2 = player2.popleft()

            if len(player1) >= c1 and len(player2) >= c2:
                p1, p2 = game(tuple(player1)[:c1], tuple(player2)[:c2]) #, game_nbr+1)
                if len(p1) < len(p2):
                    player2.append(c2)
                    player2.append(c1)
                else:
                    player1.append(c1)
                    player1.append(c2)
            else:
                if c1 < c2:
                    player2.append(c2)
                    player2.append(c1)
                else:
                    player1.append(c1)
                    player1.append(c2)

        return player1, player2


    p1, p2 = game(player1, player2)
    deck = reversed(p1 or p2)

    return sum((i+1)*c for i,c in enumerate(deck))

def main():
    """ main """
    player1 = []
    player2 = []
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

    print('part_one', part_one(deque(player1), deque(player2)))

    print('part_two', part_two(tuple(player1), tuple(player2)))


main()
