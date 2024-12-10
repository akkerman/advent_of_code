"""Day 8: Resonant Collinearity."""
import sys

Antenna = tuple[str, int, int]
Coord = tuple[int, int]

def print_map(antennas: set[Antenna], antinodes: set[Coord], size: tuple[int, int]):
    """Print the map with antennas and antinodes."""
    coords_to_name: dict[Coord, str] = {(r,c): n for n,r,c in antennas}
    for r in range(1, 1+size[0]):
        for c in range(1, 1+size[1]):
            if (r,c) in antinodes:
                print('#', end='')
            elif (r,c) in coords_to_name:
                print(coords_to_name[(r,c)], end='')
            else:
                print('.', end='')
        print()


def part_one(antennas: set[Antenna], names: set[str], size: tuple[int, int]):
    """Determine unique locations of antinodes with double the distance between antennas."""
    antinodes: set[Coord] = set()

    def is_within_grid(r:int,c:int):
        return 1 <= r <= size[0] and 1 <= c <= size[1]

    def add_antinode(r:int,c:int):
        if is_within_grid(r, c):
            antinodes.add((r,c))

    for name in names:
        coords = [(r,c) for n,r,c in antennas if n == name]
        for (r1, c1), (r2, c2) in ((p1, p2) for p1 in coords for p2 in coords if p1 != p2):
            add_antinode(r1+(r1-r2), c1+(c1-c2))
            add_antinode(r2+(r2-r1), c2+(c2-c1))

    return len(antinodes)


def part_two(antennas: set[Antenna], names: set[str], size: tuple[int, int]):
    """Determine unique locations of antinodes for all antenna distances within the map."""
    antinodes: set[Coord] = set()

    def is_within_grid(r:int,c:int):
        return 1 <= r <= size[0] and 1 <= c <= size[1]

    def add_antinode(r:int,c:int):
        if is_within_grid(r, c):
            antinodes.add((r,c))
            return True
        return False


    for name in names:
        coords = [(r,c) for n,r,c in antennas if n == name]

        for (r1, c1), (r2, c2) in ((p1, p2) for p1 in coords for p2 in coords if p1 != p2):
            m = 1
            while add_antinode(r1+(r2-r1)*m, c1+(c2-c1)*m):
                m += 1

            m = 1
            while add_antinode(r2+(r1-r2)*m, c2+(c1-c2)*m):
                m += 1

    return len(antinodes)


def main():
    """ main """
    antennas: set[Antenna] = set()
    names: set[str] = set()
    rows = 0
    cols = 0
    for line in sys.stdin:
        line = line.strip()
        cols = len(line)
        rows += 1

        for col, name in enumerate(line):
            if name != '.':
                antennas.add((name, rows, 1+col))
                names.add(name)
        

    print('part_one', part_one(antennas, names, (rows, cols)))

    print('part_two', part_two(antennas, names, (rows, cols)))

main()
