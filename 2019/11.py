"""Day 11: Space Police."""
import fileinput
from collections import defaultdict
from intcode import computer, IO

Coord = tuple[int, int]
Dir = tuple[int, int]

MOVE = 0
PAINT = 1
LEFT = 0
RIGHT = 1
BLACK = 0
WHITE = 1
class Robot(IO):
    def __init__(self):
       self.grid: dict[Coord, int] = defaultdict(lambda: BLACK)
       self.direction: Dir = (-1, 0)
       self.position: Coord = (0, 0)
       self.state = PAINT
       self.painted: set[Coord] = set()

    def input(self) -> int:
        return self.see()

    def output(self, value:int) -> None:
        self.instruct(value)

    def instruct(self, value:int) -> None:
        if self.state == PAINT:
            self.grid[self.position] = value
            self.painted.add(self.position)
        if self.state == MOVE:
            dr, dc = self.direction
            if value == RIGHT:
                self.direction = (dc, -dr)
            if value == LEFT:
                self.direction = (-dc, dr)

            r, c = self.position
            dr, dc = self.direction
            self.position = (r+dr, c+dc)

        self.state^=1

    def see(self) -> int:
        return self.grid[self.position]


def part_one(program:list[int]):
    """Solution to part one."""
    return computer(defaultdict(int, enumerate(program)), Robot())


def part_two(program:list[int]):
    """Solution to part two."""

    robot = Robot()
    robot.grid[(0,0)] = WHITE
    computer(defaultdict(int, enumerate(program)), robot)

    min_row = min(( r for r, _ in robot.painted ))
    max_row = max(( r for r, _ in robot.painted ))
    min_col = min(( c for _, c in robot.painted ))
    max_col = max(( c for _, c in robot.painted ))

    for r in range(min_row, max_row+1):
        for c in range(min_col, max_col+1):
            print('█' if robot.grid[(r,c)] == WHITE else ' ', end='')
        print()


def main():
    """Parse input file, pass to puzzle solvers."""
    program = []
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))
        break

    print('part_one', part_one(program.copy()))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
