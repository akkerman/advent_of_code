"""Day 8: Space Image Format."""
import fileinput
from collections import Counter

def part_one(input:str):
    """Solution to part one."""
    offset = 3*2 if len(input) < 100 else 25*6
    zeros=offset+1
    count:Counter[str]=Counter()
    while len(input):
        c = Counter(input[:offset])
        if c['0'] < zeros:
            zeros = c['0']
            count = c
        input=input[offset:]
    
    return int(count['1']) * int(count['2'])


BLACK='0'
WHITE='1'
TRANSPARENT='2'

def overlay(pixels:str):
    for p in pixels:
        if p == BLACK:
            return ' '
        if p == WHITE:
            return '█'
    return TRANSPARENT

def part_two(input:str):
    """Solution to part two."""
    offset = 6*25
    image = [input[i:i+offset] for i in range(0, len(input), offset)]
    pixels = [overlay(p) for p in zip(*image)]
    [print(''.join(pixels[i:i+25])) for i in range(0, len(pixels), 25)]


def main():
    """Parse input file, pass to puzzle solvers."""
    input=""
    for line in fileinput.input():
        input += line.strip()


    print('part_one', part_one(input))

    print('part_two', part_two(input))


main()
