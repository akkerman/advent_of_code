"""Day 16: Reindeer Maze."""
import sys
import heapq
from collections import defaultdict, deque

Coord = tuple[int, int] # row, col
Direction = tuple[int, int] # row, col
Step = tuple[Coord, Direction]
CostCD = tuple[int, Coord, Direction]

def move(coord: Coord, direction: Direction) -> Coord:
    return (coord[0] + direction[0], coord[1] + direction[1])

def part_one(path: set[Coord], start: Coord, end: Coord) -> int:
    """Determine the lowest score a Reindeer could get to reach the end of the maze."""
    queue: list[CostCD] = [(0, start, (0,1))]
    visited: set[Step] = set()

    while queue:
        next = heapq.heappop(queue)
        cost, coord, dir = next
        if coord == end: return cost
        if (coord, dir) in visited: continue
        visited.add((coord, dir))

        dr, dc = dir
        dir_cost = [ ((dr, dc), 1), ((dc, -dr), 1001), ((-dc, dr), 1001) ]
        for next_dir, added_cost in dir_cost:
            next = move(coord, next_dir)
            if next not in path: continue
            heapq.heappush(queue, (cost+added_cost, next, next_dir))
    
    return -1


def print_path(path: set[Coord], tiles: set[Coord], wall: set[Coord]): 
    for r in range(17):
        for c in range(17):
            if (r,c) in tiles and (r,c) in wall:
                print('X', end='')
            elif (r,c) in wall:
                print('#', end='')
            elif (r,c) in tiles:
                print('O', end='')
            elif (r,c) in path:
                print('.', end='')
            else:
                print(' ', end='')
        print()

def part_two(path: set[Coord], start: Coord, end: Coord, wall: set[Coord], target_cost: int) -> int:
    """Solution to part two."""
    queue: list[CostCD] = [(0, start, (0,1))]
    visited: set[tuple[Coord, Coord]] = set()

    back_path: defaultdict[Step, set[Step]] = defaultdict(set)
    lowest_cost: defaultdict[Step, int] = defaultdict(lambda: 8**85)

    end_states: set[Step] = set()
    while queue:
        next = heapq.heappop(queue)
        cost, coord, dir = next
        if coord == end: 
            if cost == target_cost:
                end_states.add((coord, dir))
            continue
        if (coord, dir) in visited: continue
        visited.add((coord, dir))
        lowest_cost[(coord, dir)] = min(cost, lowest_cost[(coord, dir)])

        dr, dc = dir
        dir_cost = [ ((dr, dc), 1), ((dc, -dr), 1001), ((-dc, dr), 1001) ]
        for next_dir, added_cost in dir_cost:
            next = move(coord, next_dir)
            if next not in path: continue
            heapq.heappush(queue, (cost+added_cost, next, next_dir))
            back_path[(next, next_dir)].add((coord, dir))

    tiles: set[Coord] = set()
    back_queue: deque[Step] = deque(end_states)
    visited.clear()
    while back_queue:
        coord, dir = back_queue.popleft()
        tiles.add(coord)
        if (coord, dir) in visited: continue
        visited.add((coord, dir))
        if coord == start: continue
        lowest = min(lowest_cost[cd] for cd in back_path[coord, dir])
        for prev in [cd for cd in back_path[coord, dir] if lowest_cost[cd] == lowest]:
            back_queue.append(prev)
    
    for coord, back in back_path.items():
        if not coord == (9,7): continue
        [print(p, lowest_cost[p]) for p in back]
    # [print(lowest_cost[p]) for p in back for x, back in back_path.items() if x == ]

    [print(back_path[p]) for p in back_path if p[0] == (9,7)]
    print(lowest_cost[(9,6), (0,1)])
    print(lowest_cost[(9,8), (0,-1)])

    print_path(path, tiles, wall)
    return len(tiles)

def main():
    """Parse input file, pass to puzzle solvers."""
    path: set[Coord] = set()
    wall: set[Coord] = set()
    row = 0
    start = (-1, -1)
    end = (-1, -1)
    for line in sys.stdin:
        line = line.strip()
        path.update((row,c) for c,d in enumerate(line) if d != '#')
        wall.update((row,c) for c,d in enumerate(line) if d == '#')
        if 'S' in line:
            start = (row, line.index('S'))
        if 'E' in line:
            end = (row, line.index('E'))

        row += 1

    cost = part_one(path, start, end)
    print('part_one', cost)

    print('part_two', part_two(path, start, end, wall, cost))


main()
