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

    def neighbors(c: Coord) -> list[Coord]:
        """Get adjacent coordinates."""
        x, y = c
        nbs: list[Coord] = []
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx, ny = x + dx, y + dy
            nbs.append((nx, ny))
        return nbs

    immovable: set[Coord] = set()
    for coord, du in nodes.items():
        if du.used == 0:
            continue  # empty node kan altijd data ontvangen
        if not any(
            du.used <= nodes[n].size
            for n in neighbors(coord)
            if n in nodes
        ):
            immovable.add(coord)

    max_available = max(
        du.avail
        for _, du in nodes.items()
    )
    print('max available', max_available)

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

def part_two(disk_usages: list[DiskUsage]):
    """Solution to part two."""


    gain_access = top_right_coord(disk_usages)
    print('goal data at', gain_access)

    print_grid(disk_usages)


    

    return 'todo'


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
