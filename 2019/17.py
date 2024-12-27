"""Day 17: Set and Forget."""
import fileinput
from collections import defaultdict, deque
from intcode import computer, IO
from utils import find_occurences

SCAFFOLD = ord('#')
SPACE = ord('.')
NEWLINE = ord('\n')
NL = ord('\n')

FACING_UP = ord('^')
FACING_DOWN = ord('v')
FACING_LEFT = ord('<')
FACING_RIGHT = ord('>')
FACING_DEAD = ord('X')

directions = {
        '^': (-1, 0),
        'v': (1, 0),
        '<': (0, -1),
        '>': (0, 1),
        }

class Vacuum(IO):
    def __init__(self):
        self.row = 0
        self.col = 0
        self.scaffolds = set[tuple[int, int]]()
        self.locations = dict[tuple[int, int], str]()
        self.instructions = deque[int]()
        self.print_msg = False
        self.last_output = 0

    def add_instructions(self, instructions: str):
        for i, instruction in enumerate(instructions.split(',')):
            if i > 0:
                self.instructions.append(ord(','))
            if instruction in ('A', 'B', 'C', 'R', 'L', 'y', 'n'):
                self.instructions.append(ord(instruction))
            else:
                for c in instruction:
                    self.instructions.append(ord(c))
        self.instructions.append(NL)


    def input(self):
        instruction = self.instructions.popleft()
        self.print_msg = False
        return instruction

    def output(self, value: int):
        self.last_output = value
        if value == NEWLINE:
            self.row += 1
            self.col = 0
        elif value == SCAFFOLD:
            self.scaffolds.add((self.row, self.col))
            self.col += 1
        elif value == SPACE:
            self.col += 1
        elif value in (FACING_UP, FACING_DOWN, FACING_LEFT, FACING_RIGHT, FACING_DEAD):
            self.locations[(self.row, self.col)] = chr(value)
            self.col += 1
        else:
            # print(f'Unexpected output: {value} = {chr(value)}')
            if not self.print_msg:
                print()
            self.print_msg = True
            print(chr(value), end='')

    def intersections(self):
        """Find scaffold intersections."""
        intersections = list[tuple[int, int]]()
        for r, c in self.scaffolds:
            if (r+1, c) not in self.scaffolds:
                continue
            if (r-1, c) not in self.scaffolds:
                continue
            if (r, c+1) not in self.scaffolds:
                continue
            if (r, c-1) not in self.scaffolds:
                continue
            intersections.append((r, c))
        return intersections

    def print_scaffold(self):
        """Print ascii representation of the scaffolds."""
        min_r = min(r for r,_ in self.scaffolds)
        max_r = max(r for r,_ in self.scaffolds)
        min_c = min(c for _,c in self.scaffolds)
        max_c = max(c for _,c in self.scaffolds)

        print('bounds', min_r, max_r, min_c, max_c)

        cross = self.intersections()
        for r in range(min_r, max_r+1):
            for c in range(min_c, max_c+1):
                if (r,c) in self.locations:
                    print(self.locations[(r,c)], end='')
                elif (r,c) in cross:
                    print('O', end='')
                elif (r,c) in self.scaffolds:
                    print('#', end='')
                else:
                    print('.', end='')
            print()

    def walk_scaffolds(self):
        """Walk the scaffold, return long list of instructions."""
        assert len(self.locations) == 1
        start = next(iter(self.locations))
        dir = directions[self.locations[start]]

        instructions = list[str]()

        q = deque[tuple[int,int]]()
        q.append(start)

        steps = 0
        while q:
            r,c = q.popleft()
            dr,dc = dir
            if (r+dr, c+dc) in self.scaffolds:
                steps += 1
                q.append((r+dr, c+dc))
                continue

            if steps > 0:
                instructions.append(str(steps))
                steps = 0

            ndr, ndc = dc, -dr
            if (r+ndr, c+ndc) in self.scaffolds:
                instructions.append('R')
                dir = (ndr,ndc)
                q.append((r,c))
                continue

            ndr, ndc = -dc, dr
            if (r+ndr, c+ndc) in self.scaffolds:
                instructions.append('L')
                dir = (ndr,ndc)
                q.append((r,c))
                continue

        return instructions

    def find_function(self, instructions: str) -> tuple[str, list[int]]:
        """Find the longest function that fits in the instructions."""
        function = ''
        locations = list[int]()
        print('find_function', instructions)

        for length in range(2, 21):
            fun = instructions[:length]
            if fun[-1] == ',':
                fun = fun[:-1]

            locs = find_occurences(fun, instructions)
            if len(locs) > 1:
                function = fun
                locations = locs

        return function, locations

    def generate_instructions(self) -> tuple[str,str,str,str]:
        """Distribute instructions to A, B, C."""
        main: str = ''
        fn_a: str = ''
        lc_a: list[int] = []
        fn_b: str = ''
        lc_b: list[int] = []
        fn_c: str = ''
        lc_c: list[int] = []

        instructions = ','.join(self.walk_scaffolds())

        fn_a, lc_a = self.find_function(instructions)
        withoutA = instructions.replace(fn_a+',', '').replace(fn_a, '')
        fn_b, lc_b = self.find_function(withoutA)
        withoutB = withoutA.replace(fn_b+',', '').replace(fn_b, '')
        fn_c, lc_c = self.find_function(withoutB)

        print('fn_a', fn_a, lc_a)
        print('fn_b', fn_b, lc_b)
        print('fn_c', fn_c, lc_c)

        main = instructions
        main = main.replace(fn_a, 'A')
        main = main.replace(fn_b, 'B')
        main = main.replace(fn_c, 'C')

        return main, fn_a, fn_b, fn_c


def part_one(intersections: list[tuple[int, int]]):
    """Calculate the sum of the alignment parameters of the scaffold intersections."""
    return sum(r * c for r, c in intersections)





def part_two(program: list[int], p1_vacuum: Vacuum):
    """Solution to part two."""
    vacuum = Vacuum()
    program[0] = 2

    vacuum.add_instructions('A,B,C') # Main
    vacuum.add_instructions('L,4') # A
    vacuum.add_instructions('L,6') # B
    # vacuum.add_instructions('L,8') # C   
    vacuum.add_instructions('n') # Continuous video feed

    print('instructions', vacuum.instructions)
    computer(defaultdict(int, enumerate(program)), vacuum)
    vacuum.print_scaffold()
    return vacuum.last_output


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    vacuum = Vacuum()
    computer(defaultdict(int, enumerate(program)), vacuum)
    print('part_one', part_one(vacuum.intersections()))

    # print('generating instructions')
    print()
    print('-'*80)
    print(vacuum.generate_instructions())

    # print('part_two', part_two(program, vacuum))


if __name__ == '__main__':
   main()
