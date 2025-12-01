"""2025 Day 1: Secret Entrance"""
import fileinput
import re

def part_one(instructions: list[tuple[str, int]]):
    """Solution to part one."""
    pos = 50
    zeroes = 0
    for d, n in instructions:
        if d == 'L':
            pos = (pos - n) % 100
        elif d == 'R':
            pos = (pos + n) % 100

        if pos == 0:
            zeroes += 1

    return zeroes



def part_two(instructions: list[tuple[str, int]]):
    """Solution to part two."""
    pos = 50
    zeroes = 0
    for d, n in instructions:
        for _ in range(n):
            if d == 'L':
                pos -= 1
            elif d == 'R':
                pos += 1

            pos %= 100

            if pos == 0:
                zeroes += 1

    return zeroes


re_dir = re.compile(r'([LR])(\d+)')
def main():
    """Parse input file, pass to puzzle solvers."""
    instructions:list[tuple[str, int]] = []
    for line in fileinput.input():
        line = line.strip()
        if m := re_dir.match(line):
            d, n = m.groups()
            instructions.append((d, int(n)))

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()
