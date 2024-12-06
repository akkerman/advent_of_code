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

def part_two(map:List[Coord], guard_pos:Coord, max:Coord, path: Set[Coord]) -> Set[Coord]:
    """ part two """

    possibilities: Set[Coord] = set()
    for (r1, c1) in map:
        for (r2, c2) in map:
            if r1 < r2 and c1 < c2:
                # main diagonal
                p1 = (r1+1, c2+1)
                p2 = (r2-1, c1-1)
                if p1 in map and p2 not in map:
                    possibilities.add(p2)
                if p1 not in map and p2 in map:
                    possibilities.add(p1)

            if r1 > r2 and c1 < c2:
                # anti diagonal
                p1 = (r2-1, c1+1)
                p2 = (r1+1, c2-1)
                if p1 in map and p2 not in map:
                    possibilities.add(p2)
                if p1 not in map and p2 in map:
                    possibilities.add(p1)
            

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
    # too high: 13823
    # nope: 7644
    possibilities = part_two(map, guard_pos, (r, max_c), walking_path)
    print_map(map, guard_pos, (r, max_c), possibilities)
    print(possibilities)
    print('part_two', len(possibilities))


main()
