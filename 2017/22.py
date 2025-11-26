"""2017 Day 22: Sporifica Virus"""
import fileinput

Coord = tuple[int, int]
Dir = tuple[int, int]
Turn = str

UP: Dir = (0, -1)
DOWN: Dir = (0, 1)
LEFT: Dir = (-1, 0)
RIGHT: Dir = (1, 0)

L: Turn = 'L'
R: Turn = 'R'

TURNS: dict[tuple[Dir, Turn], Dir] = {
        (UP, L): LEFT,
        (UP, R): RIGHT,
        (DOWN, L): RIGHT,
        (DOWN, R): LEFT,
        (RIGHT, L): UP,
        (RIGHT, R): DOWN,
        (LEFT, L): DOWN,
        (LEFT, R): UP,
        }
            

def move(pos: Coord, dir: Dir) -> Coord:
    """Move from pos in dir."""
    return (pos[0] + dir[0], pos[1] + dir[1])

def part_one(infected: set[Coord], start: Coord):
    """Solution to part one."""
    dir: Dir = UP
    pos: Coord = start
    infections: int = 0

    for _ in range(10000):
        if pos in infected:
            dir = TURNS[(dir, R)]
            infected.remove(pos) # clean
        else:
            dir = TURNS[(dir, L)]
            infected.add(pos) # infect
            infections += 1

        pos = move(pos, dir)

    return infections


def part_two(infected: set[Coord], start: Coord):
    """Solution to part two."""
    dir: Dir = UP
    pos: Coord = start
    infections: int = 0
    weakened: set[Coord] = set()
    flagged: set[Coord] = set()

    for _ in range(10000000):
        if pos in infected:
            dir = TURNS[(dir, R)]
            infected.remove(pos)
            flagged.add(pos)
        elif pos in weakened:
            weakened.remove(pos)
            infected.add(pos)
            infections += 1
        elif pos in flagged:
            flagged.remove(pos)
            dir = TURNS[(dir, L)]
            dir = TURNS[(dir, L)]
        else: # clean
            dir = TURNS[(dir, L)]
            weakened.add(pos)

        pos = move(pos, dir)

    return infections


def main():
    """Parse input file, pass to puzzle solvers."""
    infected: set[Coord] = set()

    x:int = 1; y:int = 1
    for line in fileinput.input():
        x = 1
        for char in line.strip():
            if char == '#':
                infected.add((x, y))
            x += 1
        y += 1


        
    center: Coord = (x // 2, y // 2)

    print('part_one', part_one(infected.copy(), center))

    print('part_two', part_two(infected.copy(), center))


if __name__ == '__main__':
    main()
