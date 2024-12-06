# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Set, Tuple

Coord = Tuple[int,int]

next_direction = {
    (-1,0): (0,1),
    (0,1): (1,0),
    (1,0): (0,-1),
    (0,-1): (-1,0),
}


def part_one(map:List[Coord], guard_pos:Coord, max:Coord) -> int:
    """ part one """
    positions: Set[Coord] = set()
    def is_obstacle(pos:Coord) -> bool:
        return pos in map
    def is_valid(pos:Coord) -> bool:
        return pos[0] >= 0 and pos[0] < max[0] and pos[1] >= 0 and pos[1] < max[1]

    dir = (-1,0)

    while True:
        positions.add(guard_pos)
        next_pos = (guard_pos[0] + dir[0], guard_pos[1] + dir[1])
        if is_obstacle(next_pos):
            dir = next_direction[dir]
            continue
        if not is_valid(next_pos):
            return len(positions)
        guard_pos = next_pos

def part_two(list) -> int:
    """ part two """
    return -1



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

    # off by one!!
    print('part_one', part_one(map, guard_pos, (r, max_c)))

    print('part_two', part_two([]))


main()
