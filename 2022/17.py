from typing import TypeAlias, Tuple, List
import sys

# 7 units wide
# jetpattern repeats after end
# rock appears
#  - two from the left
#  - three from bottom/highest rock

Stone: TypeAlias = Tuple[int, int]
Direction: TypeAlias = Tuple[int, int]
Rock: TypeAlias = List[Stone]

mins: Rock = [(1, 3), (1, 4), (1, 5), (1, 6)]
plus: Rock = [(3, 4), (2, 3), (2, 4), (2, 5), (1, 4)]
hook: Rock = [(3, 5), (2, 5), (1, 3), (1, 4), (1, 5)]
pole: Rock = [(4, 3), (3, 3), (2, 3), (1, 3)]
boxx: Rock = [(2, 3), (2, 4), (1, 3), (1, 4)]
botm: Rock = [(0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7)]


rocks: List[Rock] = [mins, plus, hook, pole, boxx]

directions = {
        '<': (0, -1),
        '>': (0, 1),
        'D': (-1, 0),
        'U': (1, 0)
}

def next_rock(iteration: int):
    """ get the next rock that falls from above """
    return rocks[(iteration-1) % 5]


def place(rock: Rock, height: int):
    """ place a rock on the indicated height """
    return [(height+x, y) for (x, y) in rock]


def move_part(part: Stone, direction: Direction):
    """ move part of a rock"""
    c1, c2 = part
    d1, d2 = direction
    return (c1+d1, c2+d2)

def move_rock(rock, direction):
    """ move a rock into a direction """
    return [move_part((x,y), direction) for (x, y) in rock]


def trough_wall(rock: Rock):
    """ check if a rock touches the left wall """
    for (_, pos) in rock:
        if pos < 1 or 7 < pos:
            return True
    return False


def through_other_rocks(chamber, rock):
    """ check if the rock intersects with another rock in the chamber """
    for stone in rock:
        if stone in chamber:
            return True
    return False


def highest(rock):
    """ get the highest point of the rock """
    height = 0
    for (pos, _) in rock:
        height = max(height, pos)
    return height


def part_one(jetpattern):
    """ part one """
    chamber = set()
    chamber.update(botm)
    dropfrom = 3
    num_rocks = 1
    iteration = 0
    while num_rocks <= 2022:
        rock = place(next_rock(num_rocks), dropfrom)
        num_rocks += 1
        print('initialrock', rock)
        while True:
            ltgt = jetpattern[iteration]
            direction = directions[ltgt]
            iteration = (iteration + 1) % len(jetpattern)
            moved = move_rock(rock, direction)

            if not (trough_wall(moved) or through_other_rocks(chamber, moved)):
                rock = moved

            # print(ltgt, rock, moved)

            direction = directions['D']
            moved = move_rock(rock, direction)

            if not (trough_wall(moved) or through_other_rocks(chamber, moved)):
                rock = moved

            # print('D', rock, moved)
            if rock != moved:
                chamber.update(rock)
                dropfrom = max(dropfrom, highest(rock)+3)
                break

    return dropfrom - 3

def part_two(jetpattern):
    """ part two """
    pass


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))

main()
