"""Day 10: Monitoring Station."""
import fileinput
from collections import defaultdict
import math

def part_one_vector(asteroids: list[tuple[int,int]]):
    """Determine the best asteroid to place a monitoring station."""
    unit_vectors = set[tuple[float,float]]()

    max_asteroids = 0
    best_asteroid = (0,0)

    for a in asteroids:
        unit_vectors.clear()
        for b in asteroids:
            if a == b:
                continue

            x1, y1 = a
            x2, y2 = b
            length = math.sqrt((x2-x1)**2 + (y2-y1)**2)
            normalized = (1000 * (x2 - x1) // length, 1000 *(y2 - y1) // length)
            unit_vectors.add(normalized)

        if len(unit_vectors) > max_asteroids:
            max_asteroids = len(unit_vectors)
            best_asteroid = a

    return max_asteroids, best_asteroid

def part_one_angle(asteroids: list[tuple[int,int]]):
    """Determine the best asteroid to place a monitoring station."""
    angles = set[float]()

    max_asteroids = 0
    best_asteroid = (0,0)

    for a in asteroids:
        angles.clear()
        for b in asteroids:
            if a == b:
                continue

            x1, y1 = a
            x2, y2 = b
            angle = math.atan2(y2 - y1, x2 - x1)
            angles.add(angle)

        if len(angles) > max_asteroids:
            max_asteroids = len(angles)
            best_asteroid = a

    return max_asteroids, best_asteroid

def manhattan_distance(a: tuple[int,int], b: tuple[int,int]) -> int:
    """Return the Manhattan distance between two points."""
    x1, y1 = a
    x2, y2 = b
    return abs(x2 - x1) + abs(y2 - y1)

def normalize_angle(angle: float) -> float:
    """Normalize an angle to be between 0 and 2pi."""
    normalized = (angle - 3* math.pi / 2) % (2 * math.pi)
    return normalized



def part_two(asteroids: list[tuple[int,int]], station: tuple[int,int] = (11,13)):
    """Solution to part two."""
    angles = defaultdict[float, list[tuple[int,int]]](lambda: [])

    for asteroid in asteroids:
        if station == asteroid:
            continue

        xs, ys = station
        xa, ya = asteroid
        angle = math.atan2(ya - ys, xa - xs)
        angles[angle].append(asteroid)

    for k,v in angles.items():
        angles[k] = sorted(v, key=lambda x: manhattan_distance(station, x))

    sorted_angles = sorted(angles.keys(), key=normalize_angle)
    
    vaporized_asteroid = angles[sorted_angles[199]][0]
    x, y = vaporized_asteroid

    return x * 100 + y, vaporized_asteroid


def main():
    """Parse input file, pass to puzzle solvers."""
    asteroids = list[tuple[int,int]]() 
    y = 0
    for line in fileinput.input():
        line = line.strip()
        for x, c in enumerate(line):
            if c == '#':
                asteroids.append((x, y))
        y += 1

    print('part_one', *part_one_vector(asteroids))

    nums, station = part_one_angle(asteroids)
    print('part_one', nums, station)

    # too low: 1808
    print('part_two', *part_two(asteroids, station))


if __name__ == '__main__':
    main()
