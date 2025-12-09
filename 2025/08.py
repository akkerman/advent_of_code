"""2025 Day 8: Playground"""
import fileinput
import math

Coord = tuple[int, int, int]
JunctionBoxId = int
Pair = tuple[JunctionBoxId, JunctionBoxId]
Circuit = set[JunctionBoxId]

def euclidean_distance(a: Coord, b: Coord) -> float:
    """Calculate Euclidean distance between two 3D coordinates."""
    return ((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2 + (a[2] - b[2]) ** 2) ** 0.5

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
    circuits: dict[JunctionBoxId, Circuit] = {}
    last_pair: Pair = (-1, -1)

    for a, b in pairs:
        last_pair = (a, b)

        circuit_a = circuits.get(a)
        circuit_b = circuits.get(b)

        if circuit_a is None and circuit_b is None:
            circuit = {a, b}
            circuits[a] = circuits[b] = circuit

        elif circuit_a is not None and circuit_b is not None:
            if circuit_a is not circuit_b:
                merged = circuit_a | circuit_b
                for jb in merged:
                    circuits[jb] = merged

        elif circuit_a is not None:
            circuit_a.add(b)
            circuits[b] = circuit_a

        else:
            assert circuit_b is not None
            circuit_b.add(a)
            circuits[a] = circuit_b

        circuit = circuits[a]
        if len(circuit) == num_junction_boxes:
            break

    unique_circuits = list({id(c): c for c in circuits.values()}.values())

    return unique_circuits, last_pair

def part_one(locations: list[Coord], pairs: list[Pair]):
    """Solution to part one."""
    max_pairs = 10 if len(locations) < 1000 else 1000
    circuits, _ = create_circuits(pairs[:max_pairs], num_junction_boxes = len(locations))
    unique_circuits = {frozenset(s) for s in circuits}
    sorted_circuits = sorted(unique_circuits, key=lambda x: len(x), reverse=True)
    return math.prod(len(g) for g in sorted_circuits[:3])

def part_two(locations: list[Coord], pairs: list[Pair]):
    """Solution to part two."""
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


    pairs = pairs_sorted_by_distance(locations)
    print('part_one', part_one(locations, pairs))
    print('part_two', part_two(locations, pairs))


if __name__ == '__main__':
    main()
