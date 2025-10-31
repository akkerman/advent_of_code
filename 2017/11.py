"""Day 11: Hex Ed."""
import fileinput

Coord = tuple[int,int,int]

# https://www.redblobgames.com/grids/hexagons/
dirs = {
    'n':  (0, 1, -1),
    'ne': (1, 0, -1),
    'se': (1, -1, 0),
    's':  (0, -1, 1),
    'sw': (-1, 0, 1),
    'nw': (-1, 1, 0),
}


def add(p:Coord, q: Coord):
    px, py, pz = p
    qx, qy, qz = q
    
    return (px+qx, py+qy, pz+qz)

def dist(p:Coord):
    px, py, pz = p
    return (abs(px) + abs(py) + abs(pz)) // 2

def part_one(instructions: list[str]):
    """Solution to part one."""
    position: Coord = (0,0,0)

    for instr in instructions:
        dir = dirs[instr]
        position = add(position, dir)

    return dist(position)


def part_two(instructions: list[str]):
    """Solution to part two."""
    position: Coord = (0,0,0)
    max_dist = 0

    for instr in instructions:
        dir = dirs[instr]
        position = add(position, dir)
        max_dist = max(max_dist, dist(position))

    return max_dist


def main():
    """Parse input file, pass to puzzle solvers."""
    instructions: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        instructions = list(line.split(','))
        break
        

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()

def test_part_one_1():
    """Test part one."""
    assert part_one(['ne', 'ne', 'ne']) == 3
def test_part_one_2():
    assert part_one(['ne', 'ne', 'sw', 'sw']) == 0
def test_part_one_3():
    assert part_one(['ne', 'ne', 's', 's']) == 2
def test_part_one_4():
    assert part_one(['se', 'sw', 'se', 'sw', 'sw']) == 3
