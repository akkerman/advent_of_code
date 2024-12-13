"""Day 13: Claw Contraption."""
import sys
import re
from functools import cache
import heapq
from utils import perf_timer


Coord = tuple[int, int]
Machine = dict[str, Coord]

PATTERN_BUTTON = re.compile(r'Button ([AB]): X\+(\d+), Y\+(\d+)')
PATTERN_PRIZE = re.compile(r'Prize: X=(\d+), Y=(\d+)')
INF = 8**85

def move(coord: Coord, direction: Coord) -> Coord:
    """Move coord in direction."""
    return (coord[0] + direction[0], coord[1] + direction[1])

def too_far(coord: Coord, prize: Coord) -> bool:
    """Check if coord is too far from prize."""
    return coord[0] > prize[0] or coord[1] > prize[1]

def too_many_presses(presses: Coord) -> bool:
    """Check if too many presses."""
    return presses[0] > 100 or presses[1] > 100

def play(machine: Machine) -> int:
    """Find minimum token cost to play the game and get the prize."""
    A = machine['A']
    B = machine['B']
    P = machine['prize']

    @cache
    def press(coord: Coord, cost:int, presses: Coord) -> int:
        if (coord == P):
            return cost
        if too_far(coord, P): # or too_many_presses(presses):
            return INF

        return min(
            press(move(coord,A), cost + 3, (presses[0] + 1, presses[1])),
            press(move(coord,B), cost + 1, (presses[0], presses[1] + 1))
        )
    cost = press((0,0), 0, (0,0))
    return 0 if cost == INF else cost

def play_two(machine: Machine) -> int:

    ax, ay = machine['A']
    bx, by = machine['B']
    px, py = machine['prize']

    a = (px*by - py*bx) / (ax*by - ay*bx)
    b = (ax*py - ay*px) / (ax*by - ay*bx)

    return int(3*a + b) if a.is_integer() and b.is_integer() else 0

@perf_timer
def part_one(machines: list[Machine]) -> int:
    """Find the total cost to play all winnable games."""
    return sum(play(machine) for machine in machines)


@perf_timer
def part_two(machines: list[Machine]):
    """Find the total cost to play all winnable games."""
    return sum(play_two(machine) for machine in machines)


def main():
    """Parse input file, pass to puzzle solvers."""
    machines = list[Machine]()
    current: Machine = dict()
    for line in sys.stdin:
        line = line.strip()

        if 'Button' in line:
            match = PATTERN_BUTTON.match(line)
            assert match
            current[match.group(1)] = (int(match.group(2)), int(match.group(3)))

        elif 'Prize' in line:
            match = PATTERN_PRIZE.match(line)
            assert match
            current['prize'] = (int(match.group(1)), int(match.group(2)))
            machines.append(current)
            current = dict()

        
        

    print('part_one', part_one(machines))

    def move_price(machine: Machine) -> Machine:
        P = machine['prize']
        machine['prize'] = (P[0] + 10000000000000, P[1] + 10000000000000)
        return machine

    print('-'*40)
    print('part_one with part_two logic', part_two(machines))

    # too low: 432256907188
    # try:     41254731438299
    print('part_two', part_two([move_price(m) for m in machines]))


main()
