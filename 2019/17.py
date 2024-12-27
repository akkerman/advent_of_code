"""Day 17: Set and Forget."""
import fileinput
from collections import defaultdict, deque
from intcode import computer, IO
import re

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

    def find_function_candidates(self, instructions: str) -> list[str]:
        """Find all possible function inputs of max 20 characters."""
        candidates = list[str]()

        if instructions[0] == ',':
            raise ValueError('Instructions start with comma')

        fn = ''
        for i, part in enumerate(instructions.split(',')):
            if i > 0:
                fn += ','
            fn += part
            if len(fn) > 20:
                break
            candidates.append(fn)

        return candidates



    def create_main(self, instructions: str, fn_a: str, fn_b: str, fn_c: str) -> str|None:
        main_pattern = r'^A(,[A|B|C])+$'
        main = ''

        while instructions:
            if instructions.startswith(fn_a):
                main += 'A,'
                instructions = instructions[len(fn_a)+1:]
                continue
            if instructions.startswith(fn_b):
                main += 'B,'
                instructions = instructions[len(fn_b)+1:]
                continue
            if instructions.startswith(fn_c):
                main += 'C,'
                instructions = instructions[len(fn_c)+1:]
                continue
            return None # instruction does not start with any function

        main =  main[:-1] # laatste comma er af

        return main if re.match(main_pattern, main) else None



    def generate_instructions(self) -> tuple[str,str,str,str]:
        """Distribute instructions to A, B, C."""
        fn_a: str = ''
        fn_b: str = ''
        fn_c: str = ''

        instructions = ','.join(self.walk_scaffolds())

        for fn_a in self.find_function_candidates(instructions):
            withoutA = instructions.replace(fn_a, '').replace(',,', ',').strip(',')
            for fn_b in self.find_function_candidates(withoutA):
                withoutB = withoutA.replace(fn_b, '').replace(',,', ',').strip(',')
                while withoutB.startswith(fn_b):
                    withoutB = withoutB[len(fn_b)+1:]

                for fn_c in self.find_function_candidates(withoutB):

                    main = self.create_main(instructions, fn_a, fn_b, fn_c)
                    if main:
                        return main, fn_a, fn_b, fn_c


        raise ValueError('No solution found')



def part_one(intersections: list[tuple[int, int]]):
    """Calculate the sum of the alignment parameters of the scaffold intersections."""
    return sum(r * c for r, c in intersections)

def part_two(program: list[int], p1_vacuum: Vacuum):
    """Solution to part two."""
    vacuum = Vacuum()
    program[0] = 2

    main, fn_a, fn_b, fn_c = p1_vacuum.generate_instructions()

    vacuum.add_instructions(main)
    vacuum.add_instructions(fn_a)
    vacuum.add_instructions(fn_b)
    vacuum.add_instructions(fn_c)
    vacuum.add_instructions('n') # Continuous video feed

    computer(defaultdict(int, enumerate(program)), vacuum)
    # vacuum.print_scaffold()
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
    print('part_two', part_two(program, vacuum))


if __name__ == '__main__':
   main()
