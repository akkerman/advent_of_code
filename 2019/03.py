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
    return l

def is_between_coordinates(start: Coord, end: Coord, intersection: Coord) -> bool:
    x1, y1 = start
    x2, y2 = end
    zx, zy = intersection
    
    return min(x1, x2) <= zx <= max(x1, x2) and min(y1, y2) <= zy <= max(y1, y2)


def step_counter(wire: Wire, intersection: Coord) -> int:
    steps = 0
    for start, end in wire:
        if is_between_coordinates(start, end, intersection):
           return steps + length(start, intersection)
        steps += length(start, end)
    return -1


def part_two(wire1: Wire, wire2: Wire) -> int:
    """ part two """
    intersections = find_intersections(wire1, wire2)

    stepsum: List[int] = []
    for coord in intersections:
        if coord == (0, 0):
            continue
        stepsum.append(step_counter(wire1, coord) + step_counter(wire2, coord))

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

    # too high: 46152
    print('part_two', part_two(wires[0], wires[1]))


def test_part_one():
    wire1 = parse_wire('R8,U5,L5,D3')
    wire2 = parse_wire('U7,R6,D4,L4')
    assert part_one(wire1, wire2) == 6

    wire1 = parse_wire('R75,D30,R83,U83,L12,D49,R71,U7,L72')
    wire2 = parse_wire('U62,R66,U55,R34,D71,R55,D58,R83')
    assert part_one(wire1, wire2) == 159

    wire1 = parse_wire('R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51')
    wire2 = parse_wire('U98,R91,D20,R16,D67,R40,U7,R15,U6,R7')
    assert part_one(wire1, wire2) == 135

def test_part_two():
    wire1 = parse_wire('R8,U5,L5,D3')
    wire2 = parse_wire('U7,R6,D4,L4')
    assert part_two(wire1, wire2) == 30

    wire1 = parse_wire('R75,D30,R83,U83,L12,D49,R71,U7,L72')
    wire2 = parse_wire('U62,R66,U55,R34,D71,R55,D58,R83')
    assert part_two(wire1, wire2) == 610

    wire1 = parse_wire('R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51')
    wire2 = parse_wire('U98,R91,D20,R16,D67,R40,U7,R15,U6,R7')
    assert part_two(wire1, wire2) == 410


if __name__ == '__main__':
    main()
