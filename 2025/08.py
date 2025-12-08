"""2025 Day 8: Playground"""
import fileinput
import math
from utils import perf_timer
from itertools import combinations

Coord = tuple[int, int, int]
JunctionBoxId = int
Pair = tuple[JunctionBoxId, JunctionBoxId]
Circuit = set[JunctionBoxId]

def euclidean_distance(a: Coord, b: Coord) -> float:
    """Calculate Euclidean distance between two 3D coordinates."""
    return ((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2) ** 0.5

@perf_timer
def pairs_sorted_by_distance(locs: list[Coord]) -> list[Pair]:
    """Generate all unique pairs of junction box locations, sorted by distance."""
    distances = dict[Pair, float]()
    for i in range(len(locs)):
        for j in range(i + 1, len(locs)):
            dist = euclidean_distance(locs[i], locs[j])
            distances[(i, j)] = dist

    return sorted(distances, key=lambda k: distances[k])

def create_circuits(pairs:list[Pair], num_junction_boxes: int = 0) -> tuple[list[Circuit], Pair]:
    """Create circuits from all pairs or until all junction boxes are connected."""
    circuits = dict[JunctionBoxId, Circuit]()
    left, right = -1, -1
    for left, right in pairs:
        if left not in circuits and right not in circuits:
            circuits[left] = set[int]([left,right])
            circuits[right] = circuits[left]

        elif left in circuits and right in circuits:
            if circuits[left] is not circuits[right]:
                union = circuits[left] | circuits[right]
                for member in union:
                    circuits[member] = union
            # else they are already connected

        elif right in circuits:
            circuits[right].add(left)
            circuits[left] = circuits[right]

        else: 
            assert left in circuits
            circuits[left].add(right)
            circuits[right] = circuits[left]

        if len(circuits[left]) == num_junction_boxes:
            assert len(circuits[right]) == num_junction_boxes
            break

    return list(circuits.values()), (left, right)

def part_one(locations: list[Coord]):
    """Solution to part one."""
    pairs = pairs_sorted_by_distance(locations)
    max_pairs = 10 if len(locations) < 1000 else 1000
    circuits, _ = create_circuits(pairs[:max_pairs], num_junction_boxes = len(locations))
    unique_circuits = {frozenset(s) for s in circuits}
    sorted_circuits = sorted(unique_circuits, key=lambda x: len(x), reverse=True)
    return math.prod(len(g) for g in sorted_circuits[:3])

def part_two(locations: list[Coord]):
    """Solution to part two."""
    pairs = pairs_sorted_by_distance(locations)
    _, (left, right) = create_circuits(pairs, num_junction_boxes = len(locations))
    return locations[left][0] * locations[right][0]


def main():
    """Parse input file, pass to puzzle solvers."""
    locations = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert len(coord) == 3
        locations.append(coord)


    print('part_one', part_one(locations))
    print('part_two', part_two(locations))


if __name__ == '__main__':
    main()
