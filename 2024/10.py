# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


Row = int
Col = int
Height = int

Position = tuple[Row, Col, Height]


def trail_heads(topo: list[Position]):
    """ enumerate positions with height 0 """
    for (row, col, height) in topo:
        if height == 0:
            yield (row, col, height)

def make_get_neighbours(topo: list[Position]):
    """ return a function that returns the neighbours of a position """
    lookup = {(row, col): height for (row, col, height) in topo}

    def get_neighbours(r: Row, c: Col) -> list[Position]:
        """ return neigbouring positions """
        candidates = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
        return [(row, col, lookup[(row, col)]) for row, col in candidates if (row, col) in lookup]
    return get_neighbours


def make_score(topo: list[Position]):
    """ return a function that scores a trail """
    neighbours = make_get_neighbours(topo)

    def score_trail(start: Position) -> int:
        """ score a trail starting at start """
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
                    print('score', score, 'at', nr, nc)
                else: 
                    todo.append((nr, nc, nh))

                visited.add((nr, nc))

        return score

    return score_trail


def part_one(topo: list[Position]) -> int:
    """ sum of trailhead scores"""
    score = make_score(topo)
    return sum(score(start) for start in trail_heads(topo))


def part_two(lines):
    """ part two """
    return 'todo'

def as_int(s: str) -> int:
    """ convert string to int, return -1 if on error """
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
