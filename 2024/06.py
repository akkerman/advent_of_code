# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Set, Tuple, Dict

Coord = Tuple[int,int]
Direction = Tuple[int,int]

UP: Direction = (-1,0)
DOWN: Direction = (1,0)
LEFT: Direction = (0,-1)
RIGHT: Direction = (0,1)

next_direction: Dict[Direction, Direction] = {
    UP: RIGHT,
    RIGHT: DOWN,
    DOWN: LEFT,
    LEFT: UP,
}

def print_map(map:List[Coord], guard_pos:Coord, max:Coord, extra: Set[Coord]):
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

def part_one(map:List[Coord], guard_pos:Coord, max:Coord):
    """ part one """
    path: Set[Coord] = set()
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

def stuck_in_loop(map:List[Coord], guard_initial_pos: Tuple[Coord, Direction], max: Coord) -> bool:
    directional_path: Set[Tuple[Coord,Direction]] = set()
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


def part_two(map:List[Coord], guard_pos: Coord, max:Coord):
    """ part two """
    posibilities: Set[Coord] = set()
    directional_path: Set[Tuple[Coord,Direction]] = set()

    def is_obstacle(pos:Coord) -> bool:
        return pos in map
    def is_valid(pos:Coord) -> bool:
        return pos[0] >= 0 and pos[0] < max[0] and pos[1] >= 0 and pos[1] < max[1]

    dir = (-1,0)

    while True:
        directional_path.add((guard_pos, dir))
        next_pos = (guard_pos[0] + dir[0], guard_pos[1] + dir[1])
        if is_obstacle(next_pos):
            dir = next_direction[dir]
            continue
        if not is_valid(next_pos):
            return posibilities

        if stuck_in_loop(map + [next_pos], (guard_pos, dir), max):
            posibilities.add(guard_pos)
        guard_pos = next_pos



def main():
    """ main """
    map:List[Coord] = []
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
