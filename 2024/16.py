"""Day 16: Reindeer Maze."""
import sys
import heapq

Coord = tuple[int, int] # row, col, cost

NEXT_DIR = {
    (0, 1): [(1, 0), (-1, 0)],
    (1, 0): [(0, -1), (0, 1)],
    (0, -1): [(1, 0), (-1, 0)],
    (-1, 0): [(0, -1), (0, 1)],
}

CostCD = tuple[int, Coord, Coord]

def move(coord: Coord, direction: Coord) -> Coord:
    return coord[0] + direction[0], coord[1] + direction[1]

def part_one(path: set[Coord], start: Coord, end: Coord) -> int:
    """Solution to part one."""

    def neighbors(cost:int, coord: Coord, dir: Coord) -> list[CostCD]:
        nb: list[CostCD] = []
        next = move(coord, dir)
        if next in path:
            nb.append((cost+1, next, dir))

        next_dir = NEXT_DIR[dir][0]
        next = move(coord, next_dir)
        if next in path:
            nb.append((cost+1001, next, next_dir))

        next_dir = NEXT_DIR[dir][1]
        next = move(coord, next_dir)
        if next in path:
            nb.append((cost+1001, next, next_dir))
        return nb


    queue: list[CostCD] = [(0, start, (0,1))]
    visited: set[tuple[Coord, Coord]] = set()
    while queue:
        next = heapq.heappop(queue)
        print(next)
        cost, coord, dir = next
        if coord == end:
            return cost
        if (coord, dir) in visited: continue
        visited.add((coord, dir))
        for n in neighbors(cost, coord, dir):
            heapq.heappush(queue, n)
    
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
        path.update((row,c) for c,d in enumerate(line) if d == '.')
        if 'S' in line:
            start = (row, line.index('S'))
        if 'E' in line:
            end = (row, line.index('E'))
            path.add(end)

        row += 1

    # too low: 130406
    print('part_one', part_one(path, start, end))

    print('part_two', part_two(path))


main()
