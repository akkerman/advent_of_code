"""2017 Day 19: A Series of Tubes"""
import fileinput

Coord = tuple[int, int]

vert_dir = [(0, -1), (0, 1)]
horz_dir = [(-1, 0), (1, 0)]

def add(p: Coord, q: Coord) -> Coord:
    return (p[0] + q[0], p[1] + q[1])

def solve(diagram: dict[Coord, str], start: Coord) -> tuple[str, int]:
    letters: list[str] = []
    pos = start
    direction = (0, 1)  # initially going down 
    steps = 0

    while True:
        if pos not in diagram:
            return "".join(letters), steps
        char = diagram[pos]
        steps+=1

        if char == '+':
            change = horz_dir if direction in vert_dir else vert_dir
            nbs = [add(pos, d) for d in change]
            for i, nb in enumerate(nbs):
                if nb in diagram:
                    pos = nb
                    direction = change[i]
                    break

            continue

        if char.isalpha():
            letters.append(char)
            
        pos = add(pos,direction)

def main():
    """Parse input file, pass to puzzle solvers."""
    diagram: dict[Coord, str] = {}

    start: Coord = (0, 0)
    for y, line in enumerate(fileinput.input()):
        for x, char in enumerate(line.rstrip()):
            if char == ' ':
                continue
            diagram[(x, y)] = char
            if y == 0 and char == '|':
                # there is only one entry point on the top row
                start = (x, y)
      

    letters, steps = solve(diagram,start)

    print('part_one', letters)

    print('part_two', steps)


if __name__ == '__main__':
    main()
