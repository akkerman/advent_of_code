"""Day 22: Grid Computing."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

from itertools import permutations

class DiskUsage():
    """Disk usage tuple."""
    def __init__(self, fs: str, size: int, used: int, avail: int, usep: int):
        self.fs = fs
        self.size = size
        self.used = used
        self.avail = avail
        self.usep = usep


def part_one(disk_usages: list[DiskUsage]):
    """Solution to part one."""
    return sum(
            1 for a, b in permutations(disk_usages, 2)
            if a.used != 0 and a != b and a.used <= b.avail
            )


Coord = tuple[int, int]
TOP_COORD = re.compile(r'/dev/grid/node-x(\d+)-y0')
NODE_COORD = re.compile(r'/dev/grid/node-x(\d+)-y(\d+)')

diffs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
def neighbors(c: Coord) -> list[Coord]:
    """Get adjacent coordinates."""
    x, y = c
    return [(x + dx, y + dy) for dx, dy in diffs]

def find_immovable_nodes(nodes: dict[Coord,DiskUsage]) -> set[Coord]:
    """Find immovable nodes."""
    immovable: set[Coord] = set()

    max_available = max(
        du.avail
        for _, du in nodes.items()
    )
    for coord, du in nodes.items():
        if du.used == 0:
            continue  # empty node kan altijd data ontvangen
        if du.used <= max_available:
            continue  

        immovable.add(coord)
    return immovable

def print_grid(disk_usages: list[DiskUsage]):
    """Print the grid."""
    nodes: dict[Coord, DiskUsage] = {}
    max_x = 0
    max_y = 0
    for du in disk_usages:
        if (res := NODE_COORD.match(du.fs)):
            x = int(res.group(1))
            y = int(res.group(2))
            nodes[(x, y)] = du
            max_x = max(max_x, x)
            max_y = max(max_y, y)

    max_available = max(
        du.avail
        for _, du in nodes.items()
    )

    for y in range(max_y + 1):
        row = ''
        for x in range(max_x + 1):
            du = nodes[(x, y)]
            if du.used == 0:
                row += ' _ '
            elif du.used > max_available:
                row += ' # '
            else:
                row += ' . '
        print(row)

def top_right_coord(disk_usages: list[DiskUsage]) -> Coord:
    """Find the top right coordinate."""
    max_x = max(
        int(res.group(1))
        for du in disk_usages
        if (res := TOP_COORD.match(du.fs))
        )
    return (max_x, 0)

def empty_node_coord(disk_usages: list[DiskUsage]) -> Coord:
    """Find the empty node coordinate."""
    for du in disk_usages:
        if du.used == 0:
            res = NODE_COORD.match(du.fs)
            if res:
                return (int(res.group(1)), int(res.group(2)))

    raise ValueError('No empty node found')

def parse_nodes(disk_usages: list[DiskUsage]) -> dict[Coord, DiskUsage]:
    """Parse disk usages into nodes dict."""
    nodes: dict[Coord, DiskUsage] = {}
    for du in disk_usages:
        res = NODE_COORD.match(du.fs)
        if res:
            x = int(res.group(1))
            y = int(res.group(2))
            nodes[(x, y)] = du
    return nodes

def bfs(start: Coord, goal: Coord, nodes: dict[Coord, DiskUsage]) -> int:
    """Breadth first search to find shortest path."""
    queue = deque([(start, 0)])
    visited: set[Coord] = set()

    immovable = find_immovable_nodes(nodes)
    while queue:
        current, dist = queue.popleft()
        if current == goal:
            return dist
        if current in visited:
            continue
        visited.add(current)
        for neighbor in neighbors(current):
            if neighbor not in nodes:
                continue
            if neighbor in immovable or neighbor in visited:
                continue
            queue.append((neighbor, dist + 1))

    return -1  # not found

def part_two(disk_usages: list[DiskUsage]):
    """Solution to part two."""
    goal = top_right_coord(disk_usages)
    empty_node = empty_node_coord(disk_usages)
   
    nodes = parse_nodes(disk_usages)
    to_goal = bfs(empty_node, goal, nodes)
    to_start = bfs((0,0), goal, nodes)

    return to_goal + (to_start -1) * 5

    



def main():
    """Parse input file, pass to puzzle solvers."""
    disk_usages: list[DiskUsage] = []
    for line in fileinput.input():
        if not 'dev' in line:
            continue

        fs, size, used, avail, usep = line.strip().split()

        size = int(size[:-1])
        used = int(used[:-1])
        avail = int(avail[:-1])
        usep = int(usep[:-1])
        
        disk_usages.append(DiskUsage(fs, size, used, avail, usep))

    print('part_one', part_one(disk_usages))

    print('part_two', part_two(disk_usages))


if __name__ == '__main__':
    main()
