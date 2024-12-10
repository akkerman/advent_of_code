"""Day 10: Hoof It."""
import sys


Row = int
Col = int
Height = int

Position = tuple[Row, Col, Height]


def trail_heads(topo: list[Position]):
    """Enumerate positions with height 0."""
    for (row, col, height) in topo:
        if height == 0:
            yield (row, col, height)

def make_get_neighbours(topo: list[Position]):
    """Return a function that returns the neighbours of a position."""
    lookup = {(row, col): height for (row, col, height) in topo}

    def get_neighbours(r: Row, c: Col) -> list[Position]:
        """Return neigbouring positions."""
        candidates = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
        return [(row, col, lookup[(row, col)]) for row, col in candidates if (row, col) in lookup]
    return get_neighbours


def make_score(topo: list[Position]):
    """Return a function that scores a trail."""
    neighbours = make_get_neighbours(topo)

    def score_trail(start: Position, all_trails: bool = False) -> int:
        """Score a trail starting at start."""
        score = 0
        visited: set[tuple[int,int]] = set()
        todo: list[Position] = [start]
        while todo:
            r, c, h = todo.pop()
            for nr, nc, nh in neighbours(r, c):
                if not nh == h + 1 or (nr, nc) in visited:
                    continue 

                if nh == 9:
                    score += 1
                else: 
                    todo.append((nr, nc, nh))

                if not all_trails:
                    visited.add((nr, nc))

        return score

    return score_trail


def part_one(topo: list[Position]) -> int:
    """Sum of trailhead scores for all reachable summits."""
    score = make_score(topo)
    return sum(score(start) for start in trail_heads(topo))


def part_two(topo: list[Position]) -> int:
    """Sum of trailhead score for all trails."""
    score = make_score(topo)
    return sum(score(start, all_trails=True) for start in trail_heads(topo))

def as_int(s: str) -> int:
    """Convert string to int, return -1 on error."""
    try:
        return int(s)
    except ValueError:
        return -1

def main():
    """ main """
    topo: list[Position] = []
    rows = 0
    for line in sys.stdin:
        rows += 1
        topo.extend([(rows, c+1, h) for c, h in enumerate(map(as_int, list(line.strip())))])
        
        

    print('part_one', part_one(topo))

    print('part_two', part_two(topo))


main()
