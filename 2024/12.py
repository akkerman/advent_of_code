"""Day 12: Garden Groups."""
import sys
from collections import defaultdict

Plot = tuple[str, int, int] # name, row, column
Coord = tuple[int, int] # row, column
Region = tuple[str, set[Coord]] # name, region


class GardenCalculator:
    def __init__(self, garden: list[Plot]):
        self.garden = garden
        self.lookup: defaultdict[str, set[Coord]] = defaultdict(set)
        for name, row, col in garden:
            self.lookup[name].add((row, col))


    def neigbors(self, coord:Coord) -> set[Coord]:
        """All possible neighbors for a """
        row, col = coord
        return {(row, col-1), (row, col+1), (row-1, col), (row+1, col)}

    def neighbor_region(self, region:set[Coord]) -> set[Coord]:
        """All possible neighbors for a region."""
        nb:set[Coord] = set()
        for coord in region:
            nb.update(self.neigbors(coord))
        return nb - region

    def region(self, plot:Plot) -> set[Coord]:
        """Return the region of a plot."""
        name, row, col = plot
        region: set[Coord] = set()
        todo = [(row, col)]
        while todo:
            r, c = todo.pop()
            region.add((r, c))
            for nr, nc in self.neigbors((r, c)):
                if (nr, nc) in region:
                    continue
                if (nr, nc) in self.lookup[name]:
                    todo.append((nr, nc))
        return region

    def perimeter(self, region:set[Coord]) -> int:
        """Determine the perimiter of a region."""
        nb_region = self.neighbor_region(region)
        per = 0
        for r, c in region:
            nb_plot = self.neigbors((r, c))
            per += len(nb_plot & nb_region)
        return per


    def area(self, region:set[Coord]) -> int:
        """Determine the area of a garden."""
        return len(region)

    def regions(self):
        """Return all regions with name."""
        visited: set[Coord] = set()
        for plot in self.garden:
            name, r, c = plot
            if (r, c) in visited:
                continue
            region = self.region(plot)
            visited.update(region)
            yield name, region




def part_one(garden: list[Plot]):
    """Solution to part one."""
    calculator = GardenCalculator(garden)
    return sum(calculator.area(region) * calculator.perimeter(region) for _, region in calculator.regions())


def part_two(lines):
    """Solution to part two."""

    return 'todo'

def main():
    """Parse input file, pass to puzzle solvers."""
    garden: list[Plot] = []

    row = 1
    for line in sys.stdin:
        line = line.strip()
        line = [(name, row, col+1) for col, name in enumerate(line)]
        garden.extend(line)
        row += 1

    print('part_one', part_one(garden))

    print('part_two', part_two(garden))


main()
