"""Day 16: Reindeer Maze."""
import sys
import heapq

Coord = tuple[int, int] # row, col
CostCD = tuple[int, Coord, Coord]

def move(coord: Coord, direction: Coord) -> Coord:
    return (coord[0] + direction[0], coord[1] + direction[1])

def part_one(path: set[Coord], start: Coord, end: Coord) -> int:
    """Determine the lowest score a Reindeer could get to reach the end of the maze."""
    queue: list[CostCD] = [(0, start, (0,1))]
    visited: set[tuple[Coord, Coord]] = set()

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



def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    path: set[Coord] = set()
    row = 0
    start = (-1, -1)
    end = (-1, -1)
    for line in sys.stdin:
        line = line.strip()
        path.update((row,c) for c,d in enumerate(line) if d != '#')
        if 'S' in line:
            start = (row, line.index('S'))
        if 'E' in line:
            end = (row, line.index('E'))

        row += 1

    print('part_one', part_one(path, start, end))

    print('part_two', part_two(path))


main()
