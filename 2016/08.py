# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

class Screen:
    """ tiny-code-displaying-screen """
    def __init__(self, wide, tall):
        self.wide = wide
        self.tall = tall
        self.pixels = set()

    def rect(self, wide, tall):
        for row in range(tall):
            for col in range(wide):
                self.pixels.add((row, col))

    def rotCol(self, x, delta):
        col = {(row, col) for row, col in self.pixels if col == x}
        rot = {((row+delta)%self.tall, col) for row, col in col}
        self.pixels = (self.pixels - col) | rot

    def rotRow(self, y, delta):
        row = {(row, col) for row, col in self.pixels if row == y}
        rot = {(row, (col+delta) % self.wide) for row, col in row}
        self.pixels = (self.pixels - row) | rot

    def print(self):
        for row in range(self.tall):
            line = ''
            for col in range(self.wide):
                if (row, col) in self.pixels:
                    line+='#'
                else:
                    line+=' '
            print(line)

reg_rect = re.compile(r'.+ (\d+)x(\d+)')
reg_rot  = re.compile(r'.+=(\d+) by (\d+)')


def solve(instructions):
    """ part one """
    if len(instructions) < 10:
        s = Screen(wide=7, tall=3)
    else:
        s = Screen(wide=50, tall=6)

    for instr in instructions:
        if 'rect' in instr:
            m = reg_rect.match(instr)
            assert m
            w,t = m.groups()
            s.rect(int(w), int(t))
            continue

        m = reg_rot.match(instr)
        assert m
        xy, delta = m.groups()

        if 'rotate row' in instr:
            s.rotRow(int(xy), int(delta))
            continue

        if 'rotate column' in instr:
            s.rotCol(int(xy), int(delta))
            continue

        assert False

    return s

def main():
    """ main """
    instructions = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        instructions.append(line)

    screen = solve(instructions)
    print('part_one', len(screen.pixels))
    print('\npart_two')
    screen.print()


main()
