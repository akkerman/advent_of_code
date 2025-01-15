"""Day 24: Planet of Discord."""
import fileinput

Coord = tuple[int, int]

def print_grid(bugs: set[Coord]):
    """Print grid for debugging."""
    for r in range(0, 5):
        for c in range(0, 5):
            if (r, c) in bugs:  
                print('#', end='')
            else:
                print('.', end='')
        print()

def step(bugs: set[Coord]):
    def count_adjacent(coord: Coord):
        r, c = coord
        count = 0
        for dr, dc in ((-1, 0), (1, 0), (0, -1), (0, 1)):
            adj = (r + dr, c + dc)
            if adj in bugs:
                count += 1
        return count
    new_bugs = set[Coord]()

    for r in range(0, 5):
        for c in range(0, 5):
            coord = (r, c)
            if coord in bugs:
                if count_adjacent(coord) == 1:
                    new_bugs.add(coord)
            else:
                if count_adjacent(coord) in [1, 2]:
                    new_bugs.add(coord)
    return new_bugs

def biodiversity_rating(bugs: set[Coord]):
    return sum(2 ** (r * 5 + c) for r, c in bugs)

def part_one(bugs: set[Coord]):
    """Calculate the biodiversity rating for the first layout that appears twice."""
    ratings = set[int]()
    while True:
        rating = biodiversity_rating(bugs)
        if rating in ratings:
            return rating
        ratings.add(rating)
        bugs = step(bugs.copy())


def part_two(bugs: set[Coord]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    bugs = set[Coord]()
    row = 0
    for line in fileinput.input():
        line = line.strip()
        for col, c in enumerate(line):
            if c == '#':
                bugs.add((row, col))
        row +=1

    print('part_one', part_one(bugs))

    print('part_two', part_two(bugs))


if __name__ == '__main__':
    main()
