"""Day 16: Reindeer Maze."""
import sys
import heapq
from collections import defaultdict, deque
from utils import perf_timer

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

@perf_timer
def part_two(path: set[Coord], start: Coord, end: Coord, wall: set[Coord], target_cost: int) -> int:
    queue: list[CostCD] = [(0, start, (0,1))]

    end_states: set[Step] = set()

    back_path: defaultdict[Step, set[Step]] = defaultdict(set)
    lowest_cost: defaultdict[Step, int] = defaultdict(lambda: 8**85)

    while queue:
        next = heapq.heappop(queue)
        cost, coord, dir = next
        if coord == end: 
            if cost == target_cost:
                end_states.add((coord, dir))
            continue
        if cost > lowest_cost[(coord, dir)]: continue
        lowest_cost[(coord, dir)] = cost

        dr, dc = dir
        dir_cost = [ (move(coord, (dr,dc)), (dr, dc), 1), (coord, (dc, -dr), 1000), (coord, (-dc, dr), 1000) ]
        for next, next_dir, added_cost in dir_cost:
            if next not in path: continue
            current_cost = cost + added_cost
            lowest = lowest_cost[(next, next_dir)]

            if current_cost > lowest: continue
            if current_cost < lowest:
                back_path[(next, next_dir)] = set()
            lowest_cost[(next, next_dir)] = current_cost
            back_path[(next, next_dir)].add((coord, dir))
            heapq.heappush(queue, (cost+added_cost, next, next_dir))

    
    tiles: set[Coord] = set()
    back_queue: deque[Step] = deque(end_states)
    while back_queue:
        coord, dir = back_queue.popleft()
        tiles.add(coord)
        for prev in back_path[coord, dir]:
            back_queue.append(prev)

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
