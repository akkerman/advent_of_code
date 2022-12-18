import sys
from math import inf
from collections import deque

directions = [(1, 0, 0), (0, 1, 0), (0, 0, 1), (-1, 0, 0), (0, -1, 0), (0, 0, -1)]

def part_one(rocks):
    area = 0
    for x, y, z in rocks:
        for dx, dy, dz in directions:
            neighbour = (x+dx, y+dy, z+dz)
            if neighbour not in rocks:
                area += 1
    return area


def getLimitsAroundRock(rocks):
    minX = minY = minZ = inf
    maxX = maxY = maxZ = -inf

    for x, y, z in rocks:
        minX = min(minX, x - 1)
        minY = min(minY, y - 1)
        minZ = min(minZ, z - 1)
        maxX = max(maxX, x + 1)
        maxY = max(maxY, y + 1)
        maxZ = max(maxZ, z + 1)

    return minX, minY, minZ, maxX, maxY, maxZ

def part_two(rocks):
    minX, minY, minZ, maxX, maxY, maxZ = getLimitsAroundRock(rocks)

    air = set()
    fifo = deque([(minX, minY, minZ)])

    # all 1x1x1 cubes of air (water) around the rock
    while fifo:
        x, y, z = fifo.popleft()
        for dx, dy, dz in directions:
            nx, ny, nz = x+dx, y+dy, z+dz
            if minX <= nx <= maxX and minY <= ny <= maxY and minZ <= nz <= maxZ:
                neighbour = (nx, ny, nz)
                if neighbour not in rocks and neighbour not in air:
                    air.add(neighbour)
                    fifo.append(neighbour)

    # figure out if air (water) is touching rock
    area = 0
    for x, y, z in air:
        for dx, dy, dz in directions:
            neighbour = (x+dx, y+dy, z+dz)
            if neighbour in rocks:
                area += 1

    return area


def main():
    rocks = set()
    for line in sys.stdin:
        line = line.replace('\n', '')
        x, y, z = line.split(',')
        rocks.add((int(x), int(y), int(z)))

    print('part_one', part_one(rocks))

    print('part_two', part_two(rocks))


main()
