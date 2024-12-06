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


def part_one(map:List[Coord], guard_pos:Coord, max:Coord) -> Set[Coord]:
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
            return positions
        guard_pos = next_pos

def part_two(map:List[Coord], guard_pos:Coord, max:Coord, path: Set[Coord]) -> int:
    """ part two """

    possibilities = 0
    for (r1, c1) in path:
        for (r2, c2) in path:
            if r1 >= r2 or c1 >= c2:
               # skip if not diagonal
               continue

            if (r1, c2) in path and (r2, c1) in path:
                num_obstacles = 0
                # a rectangle was found
                # check bounding obstacles
                if (r1-1, c1) in map:
                    num_obstacles += 1
                if (r1, c2+1) in map:
                    num_obstacles += 1
                if (r2+1, c2) in map:
                    num_obstacles += 1
                if (r2, c1-1) in map:
                    num_obstacles += 1
                if num_obstacles == 3:
                    possibilities += 1

    return possibilities



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

    walking_path = part_one(map, guard_pos, (r, max_c))
    print('part_one', len(walking_path))

    # too low: 539
    print('part_two', part_two(map, guard_pos, (r, max_c), walking_path))


main()
