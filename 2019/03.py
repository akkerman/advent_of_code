# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
from typing import List, Tuple


Coord = Tuple[int, int]
Vector = Tuple[Coord, Coord]
Wire = List[Vector]

def find_intersections(wire1: Wire, wire2: Wire):

    horizontal1 = [v for v in wire1 if v[0][1] == v[1][1]]  # y-coördinaat gelijk
    vertical1 = [v for v in wire1 if v[0][0] == v[1][0]]    # x-coördinaat gelijk
    horizontal2 = [v for v in wire2 if v[0][1] == v[1][1]]
    vertical2 = [v for v in wire2 if v[0][0] == v[1][0]]
    
    intersections: List[Coord] = []

    # Controle horizontale van wire1 met verticale van wire2
    for h in horizontal1:
        for v in vertical2:
            h_x1, h_x2 = sorted([h[0][0], h[1][0]])
            v_y1, v_y2 = sorted([v[0][1], v[1][1]])
            if v[0][0] in range(h_x1, h_x2 + 1) and h[0][1] in range(v_y1, v_y2 + 1):
                intersections.append((v[0][0], h[0][1]))

    # Controle horizontale van wire2 met verticale van wire2
    for h in horizontal2:
        for v in vertical1:
            h_x1, h_x2 = sorted([h[0][0], h[1][0]])
            v_y1, v_y2 = sorted([v[0][1], v[1][1]])
            if v[0][0] in range(h_x1, h_x2 + 1) and h[0][1] in range(v_y1, v_y2 + 1):
                intersections.append((v[0][0], h[0][1]))

    return intersections
def part_one(wire1, wire2):
    """ part one """
    return 'todo'


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
