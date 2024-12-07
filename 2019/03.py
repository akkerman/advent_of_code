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
def part_one(wire1:Wire, wire2:Wire):
    """ part one """
    return min([abs(x) + abs(y) for x, y in find_intersections(wire1, wire2) if x != 0 and y != 0])



def length(start: Coord, end: Coord) -> int:
    l= abs(start[0] - end[0]) + abs(start[1] - end[1])
    print(f'{start} -> {end} = {l}')
    return l

def step_counter(wire: Wire, intersection: Coord) -> int:
    steps = 0
    for start, end in wire:
        if intersection == start:
            return steps
        steps += length(start, end)
    return steps


def part_two(wire1: Wire, wire2: Wire) -> int:
    """ part two """
    intersections = find_intersections(wire1, wire2)

    stepsum: List[int] = []
    for coord in intersections:
        print('\nfind steps for', coord)
        if coord == (0, 0):
            continue
        stepsum.append(step_counter(wire1, coord) + step_counter(wire2, coord))

    print (stepsum)
    return min(stepsum)



def parse_wire(line: str) -> Wire:
    """ parse wire """
    wire: Wire = []
    x, y = 0, 0
    for move in line.split(','):
        direction = move[0]
        length = int(move[1:])
        if direction == 'U':
            wire.append(((x, y), (x, y + length)))
            y += length
        elif direction == 'D':
            wire.append(((x, y), (x, y - length)))
            y -= length
        elif direction == 'L':
            wire.append(((x, y), (x - length, y)))
            x -= length
        elif direction == 'R':
            wire.append(((x, y), (x + length, y)))
            x += length
    return wire

def main():
    """ main """
    wires: List[Wire] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        wires.append(parse_wire(line))

    print('part_one', part_one(wires[0], wires[1]))

    # too high: 306080
    print('part_two', part_two(wires[0], wires[1]))

main()
