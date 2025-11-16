"""2017 Day 19: A Series of Tubes"""
import fileinput
import string

Coord = tuple[int, int]

vert_dir = [(0, -1), (0, 1)]
horz_dir = [(-1, 0), (1, 0)]

def add(p: Coord, q: Coord):
    px,py = p
    qx,qy = q
    return (px+qx,py+qy)

def solve(diagram: dict[Coord, str], start: Coord):
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
            continue

        if char in string.ascii_uppercase:
            letters.append(char)
            
        pos = add(pos,direction)

def main():
    """Parse input file, pass to puzzle solvers."""
    diagram: dict[Coord, str] = {}

    start: Coord = (0, 0)
    for y, line in enumerate(fileinput.input()):
        for x, char in enumerate(line.strip('\n')):
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
