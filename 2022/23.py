import sys

from collections import deque

# blijf als alles rondom vrij
#
# de _eerste_ richting wijzigt elke ronde Z N W E Z N W E etc.
#
# alleen als move niet mogelijk is probeert elf individueel de volgende richting
# maar begint dus weer met dezelfde richting als alle andere elfs de volgende ronde
#
# Vrij is als de naburige DRIE velden vrij zijn in die richting
# move Z ; try ZW Z ZE


directionsToCheck = {
    'A': [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)],
    'N': [(-1, -1), (0, -1), (1, -1)],
    'S': [(-1, 1), (0, 1), (1, 1)],
    'W': [(-1, -1), (-1, 0), (-1, 1)],
    'E': [(1, -1), (1, 0), (1, 1)],
}

directionsToMove = {
    'N': [(0, -1)],
    'S': [(0, 1)],
    'W': [(-1, 0)],
    'E': [(1, 0)],
}

moves = deque(['N', 'S', 'E', 'W'])

def move(elf, direction): 
    x, y = elf
    return set([(x + dx, y + dy) for (dx,dy) in directionsToCheck[direction]])


def move(elf, direction):
    x, y = elf
    dx, dy = direction
    return (x+dx, y+dy)


def part_one(elfs):
    """ get number of free squares after 10 rounds """

    for _ in range(10):
        for elf in elfs:
            if not elfs & move(elf, 'A'):
                continue

        

    
    return 'todo'


def part_two(lines):
    """ part_two """
    return 'todo'



def elfs_as_coords(lines):
    """ translate the input to a set of coordinates """
    elfs = set()
    for i, line in enumerate(lines):
        for j, entry in enumerate(line):
            if entry == '#':
                elfs.add((i, j))

    return elfs


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    elfs = elfs_as_coords(lines)

    print('part_one', part_one(elfs))

    print('part_two', part_two(lines))


main()
