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


    def game(player1, player2, game_nbr=1):
        my_game_nbr = game_nbr
        print('start game', my_game_nbr)
        states1 = set()
        states2 = set()

        round_nbr = 0
        while player1 and player2:
            round_nbr += 1
            tp1 = tuple(player1)
            tp2 = tuple(player2)
            if tp1 in states1 or tp2 in states2:
                # print('Player 1 instantly wins')
                return player1, deque(), game_nbr

            states1.add(tp1)
            states2.add(tp2)

            # print(f"\n\n-- Round {round_nbr} (Game {my_game_nbr}) --")
            # print("Player 1's deck:", player1)
            # print("Player 2's deck:", player2)
            c1 = player1.popleft()
            c2 = player2.popleft()
            # print("Player 1 plays:", c1)
            # print("Player 2 plays:", c2)

            if len(player1) >= c1 and len(player2) >= c2:
                p1, p2, game_nbr = game(deque(player1), deque(player2), game_nbr+1)
                if len(p1) < len(p2):
                    player2.append(c2)
                    player2.append(c1)
                    # print(f"Player 2 wins round {round_nbr} of game {my_game_nbr}")
                else:
                    player1.append(c1)
                    player1.append(c2)
                    # print(f"Player 1 wins round {round_nbr} of game {my_game_nbr}")
            else:
                if c1 < c2:
                    player2.append(c2)
                    player2.append(c1)
                    # print(f"Player 2 wins round {round_nbr} of game {my_game_nbr}")
                else:
                    player1.append(c1)
                    player1.append(c2)
                    # print(f"Player 1 wins round {round_nbr} of game {my_game_nbr}")


        return player1, player2, game_nbr


    p1, p2, _ = game(player1, player2)

    print("\n\n== Post-game results ==")
    print("Player 1's deck:", p1)
    print("Player 2's deck:", p2)

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

    # print('part_one', part_one(deque(player1), deque(player2)))

    print('part_two', part_two(deque(player1), deque(player2)))


main()
