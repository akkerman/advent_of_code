"""Day 6: Guard Gallivant."""
import sys

Coord = tuple[int,int]
Direction = tuple[int,int]

UP: Direction = (-1,0)
DOWN: Direction = (1,0)
LEFT: Direction = (0,-1)
RIGHT: Direction = (0,1)

next_direction: dict[Direction, Direction] = {
    UP: RIGHT,
    RIGHT: DOWN,
    DOWN: LEFT,
    LEFT: UP,
}

def print_map(map:list[Coord], guard_pos:Coord, max:Coord, extra: set[Coord]):
    """Print the laboratory map with the guard and obstacles."""
    for r in range(max[0]):
        for c in range(max[1]):
            if (r,c) in extra:
                print('O', end='')
            elif (r,c) == guard_pos:
                print('^', end='')
            elif (r,c) in map:
                print('#', end='')
            else:
                print('.', end='')
        print()

def part_one(map:list[Coord], guard_pos:Coord, max:Coord):
    """Determine distinct positions the guard will visit before leaving the mapped area."""
    path: set[Coord] = set()
    def is_obstacle(pos:Coord) -> bool:
        return pos in map
    def is_valid(pos:Coord) -> bool:
        return pos[0] >= 0 and pos[0] < max[0] and pos[1] >= 0 and pos[1] < max[1]

    dir = (-1,0)

    while True:
        path.add(guard_pos)
        next_pos = (guard_pos[0] + dir[0], guard_pos[1] + dir[1])
        if is_obstacle(next_pos):
            dir = next_direction[dir]
            continue
        if not is_valid(next_pos):
            return path
        guard_pos = next_pos

def stuck_in_loop(map:list[Coord], guard_initial_pos: tuple[Coord, Direction], max: Coord) -> bool:
    """Check if the guard will be stuck in a loop."""
    directional_path: set[tuple[Coord,Direction]] = set()
    def is_obstacle(pos:Coord) -> bool:
        return pos in map
    def is_valid(pos:Coord) -> bool:
        return pos[0] >= 0 and pos[0] < max[0] and pos[1] >= 0 and pos[1] < max[1]

    guard_pos:Coord = guard_initial_pos[0]
    dir:Direction = guard_initial_pos[1]

    while True:
        if (guard_pos, dir) in directional_path:
            return True

        directional_path.add((guard_pos, dir))
        next_pos: Coord = (guard_pos[0] + dir[0], guard_pos[1] + dir[1])
        if is_obstacle(next_pos):
            dir = next_direction[dir]
            continue
        if not is_valid(next_pos):
            return False
        guard_pos = next_pos


def part_two(map:list[Coord], guard_pos: Coord, max:Coord):
    """Determine distinct positions to place an obstruction to trap the guard in a loop."""
    posibilities: set[Coord] = set()
    directional_path: set[tuple[Coord,Direction]] = set()

    def is_obstacle(pos:Coord) -> bool:
        return pos in map
    def is_valid(pos:Coord) -> bool:
        return pos[0] >= 0 and pos[0] < max[0] and pos[1] >= 0 and pos[1] < max[1]

    dir = (-1,0)

    orig_guard_pos = guard_pos

    while True:
        directional_path.add((guard_pos, dir))
        next_pos = (guard_pos[0] + dir[0], guard_pos[1] + dir[1])
        if is_obstacle(next_pos):
            dir = next_direction[dir]
            continue
        if not is_valid(next_pos):
            return posibilities

        if stuck_in_loop(map + [next_pos], (orig_guard_pos, UP), max):
            posibilities.add(guard_pos)
        guard_pos = next_pos



def main():
    """Parse input file, pass to puzzle solvers."""
    map:list[Coord] = []
    guard_pos: Coord = (0,0)
    r = 0
    max_c = 0
    for line in sys.stdin:
        line = line.replace('\n', '')
        max_c = max(max_c, len(line))
        if '^' in line:
            guard_pos = (r, line.index('^'))
        line = [c for c in line]
        map += [(r, c) for c in range(len(line)) if line[c] == '#']
        r+=1

    path = part_one(map, guard_pos, (r, max_c))
    print('part_one', len(path))

    possibilities = part_two(map, guard_pos, (r, max_c))
    print('part_two', len(possibilities))


main()
