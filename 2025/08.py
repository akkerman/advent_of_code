"""2025 Day 8: Playground"""
import fileinput
from utils import perf_timer
import math

Coord = tuple[int, int, int]
JunctionBoxId = int
Pair = tuple[JunctionBoxId, JunctionBoxId]
Circuit = set[JunctionBoxId]


class UnionFind:
    def __init__(self, n: int):
        self.parent = list(range(n))
        self.size = [1] * n

    def find(self, i:int):
        """Find the representative element of the set containing i."""
        root = self.parent[i]
        if self.parent[root] != root:
            self.parent[i] = self.find(root)
            return self.parent[i]
      
        return root

    def unite(self, i:int, j:int):
        """Connect two elements.
        Return True if the structure change because they were not already connected.""" 
        irep = self.find(i)
        jrep = self.find(j)

        if irep == jrep:
            return False

        isize = self.size[irep]
        jsize = self.size[jrep]

        if isize < jsize:
            self.parent[irep] = jrep
            self.size[jrep] += self.size[irep]
            
        else:
            self.parent[jrep] = irep
            self.size[irep] += self.size[jrep]

        return True

class UnionFindCount(UnionFind):
    def __init__(self, n: int):
        super().__init__(n)
        self.count = n

    def connect(self, i:int, j:int):
        """Connect two elements. 
        Return True if the structure change because they were not already connected.""" 
        changed = super().unite(i, j)
        if changed:
            self.count -= 1
        return changed


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

@perf_timer
def solve(locations: list[Coord], pairs: list[Pair]):
    uf = UnionFindCount(len(locations))

    for iter, (a, b) in enumerate(pairs):
        uf.connect(a, b)
        if iter == 999:
            print('Part 1: ', math.prod(i for i in sorted(uf.size)[-3:]))
        if uf.count == 1:
            print('Part 2: ', locations[a][0] * locations[b][0])
            break


def main():
    """Parse input file, pass to puzzle solvers."""
    locations = list[Coord]()
    for line in fileinput.input():
        line = line.strip()
        coord = tuple(map(int, line.split(',')))
        assert len(coord) == 3
        locations.append(coord)

    pairs = pairs_sorted_by_distance(locations)
    solve(locations, pairs)


if __name__ == '__main__':
    main()
