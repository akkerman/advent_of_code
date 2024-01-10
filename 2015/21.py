# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from  math import ceil

# Weapons:    Cost  Damage  armour
WEAPONS = [
('Bare Hands' ,   0,  0,  0),
('Dagger'     ,   8,  4,  0),
('Shortsword' ,  10,  5,  0),
('Warhammer'  ,  25,  6,  0),
('Longsword'  ,  40,  7,  0),
('Greataxe'   ,  74,  8,  0),
]

# armour:      Cost  Damage  armour
ARMOUR = [
('Naked'      ,  0,   0, 0),
('Leather'    ,  13,  0, 1),
('Chainmail'  ,  31,  0, 2),
('Splintmail' ,  53,  0, 3),
('Bandedmail' ,  75,  0, 4),
('Platemail'  , 102,  0, 5),
]

# Rings:      Cost  Damage  armour
RINGS = [
('karate 1'   ,   0,  0 , 0),
('karate 2'   ,   0,  0 , 0),
('Damage +1'  ,  25,  1 , 0),
('Damage +2'  ,  50,  2 , 0),
('Damage +3'  , 100,  3 , 0),
('Defense +1' ,  20,  0 , 1),
('Defense +2' ,  40,  0 , 2),
('Defense +3' ,  80,  0 , 3),
]


def turns_to_defeat(p1, damage):
    hp, _, armour = p1
    actual_damage = max(1, damage - armour)
    return ceil(hp / actual_damage)

def fight(player, boss):
    player_turns = turns_to_defeat(boss, player[1])
    boss_turns = turns_to_defeat(player, boss[1])
    return 'boss' if boss_turns < player_turns else 'player'

def stats():
    for w in WEAPONS:
        for a in ARMOUR:
            for r1 in RINGS:
                for r2 in RINGS:
                    if r1 == r2:
                        continue
                    yield (sum(a) for a in list(zip(w,a,r1,r2))[1:])

# incorrect 51 and 81
def part_one(boss):
    """ part one """
    result = []

    for cost, damage, armour in stats():
        result.append((cost, fight([100, damage, armour], boss)))

    for r in result:
        print(r)

    return min(cost for cost, winner in result if winner == 'player')


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = int(line.split(': ')[1])
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
