"""Day 10: Monitoring Station."""
import fileinput
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



def part_two(lines):
    """Solution to part two."""
    return 'todo'


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

    print('part_one', part_one_vector(asteroids))
    print('part_one', part_one_angle(asteroids))

    print('part_two', part_two(asteroids))


if __name__ == '__main__':
    main()
