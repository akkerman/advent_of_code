import sys

directions_to_check = {
    'A': [(-1, -1), (-1, 0), (-1, 1),
          (1, -1), (1, 0), (1, 1),
          (0, -1),
          (0, 1),
          ],

    'N': [(-1, -1), (-1, 0), (-1, 1)],
    'S': [(1, -1), (1, 0), (1, 1)],
    'W': [(-1, -1), (0, -1), (1, -1)],
    'E': [(-1, 1), (0, 1), (1, 1)],
}

directions_to_move = {
    'N': (-1, 0),
    'S': (1, 0),
    'W': (0, -1),
    'E': (0, 1),
}

directions = ['N', 'S', 'W', 'E']

Coord = tuple[int, int]


def moves_to_check(elf: Coord, direction: str):
    """Generate coordinates an elf needs to check before moving."""
    r, c = elf
    return {(r + dr, c + dc) for (dr, dc) in directions_to_check[direction]}


def move_direction(elf: Coord, direction: str):
    """Move the elf in the indicated direction."""
    r, c = elf
    dr, dc = directions_to_move[direction]
    return (r + dr, c + dc)


def calc_empty_tiles(elves: set[Coord]):
    """Calculate empty tiles in the bounding box formed by the elves."""
    minR = min(r for r, _ in elves)
    maxR = max(r for r, _ in elves)
    minC = min(c for _, c in elves)
    maxC = max(c for _, c in elves)

    return (maxR - minR + 1) * (maxC - minC + 1) - len(elves)


def one_round(elves: set[Coord]):
    proposals = set[Coord]()
    collisions = set[Coord]()

    # first half, generate proposals and collisions
    for elf in elves:
        if not elves & moves_to_check(elf, 'A'):
            # wants to stay
            continue

        for direction in directions:
            if not elves & moves_to_check(elf, direction):
                prop = move_direction(elf, direction)
                if prop in proposals:
                    collisions.add(prop)
                proposals.add(prop)
                break

    # second half, move elves that don't collide
    moved_elves = set(elves)
    for elf in elves:
        if not elves & moves_to_check(elf, 'A'):
            # wants to stay
            continue

        for direction in directions:
            if not elves & moves_to_check(elf, direction):
                prop = move_direction(elf, direction)
                if prop not in collisions:
                    moved_elves.remove(elf)
                    moved_elves.add(prop)
                break


    # the first direction the Elves considered
    # is moved to the end of the list of directions
    directions.append(directions.pop(0))

    return moved_elves


def part_one(elves: set[Coord]):
    """Get number of free squares after 10 rounds."""

    global directions
    directions = ['N', 'S', 'W', 'E']

    for _ in range(10):
        elves = one_round(elves)

    return calc_empty_tiles(elves)


def part_two(elves: set[Coord]):
    """part_two."""

    global directions
    directions = ['N', 'S', 'W', 'E']

    rounds = 0
    while True:
        rounds += 1
        new_elves = one_round(elves)
        if elves == new_elves:
            return rounds
        elves = new_elves


def elves_as_coords(lines: list[str]):
    """Translate the input to a set of coordinates."""
    elves = set[Coord]()
    for r, line in enumerate(lines):
        for c, entry in enumerate(line):
            if entry == '#':
                elves.add((r, c))

    return elves


def main():
    """main"""
    lines = list[str]()
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    elves: set[Coord] = elves_as_coords(lines)


    print('part_one', part_one(elves.copy()))
    print('part_two', part_two(elves.copy()))


main()
