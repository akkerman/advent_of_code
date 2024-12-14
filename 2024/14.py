"""Day 14: Restroom Redoubt."""
import re
import fileinput
from collections import Counter
import math

PATTERN = re.compile(r'(-?\d+)')

# ROWS, COLS = 7, 11 # example
ROWS, COLS = 103, 101 # actual


Coords = tuple[int,int]
Velocity = tuple[int,int]
CoVe = tuple[int,int,int,int]

def print_floor(robots:list[CoVe]):
    coords = Counter([(r,c) for (r,c,_,_) in robots])
    for r in range(ROWS):
        for c in range(COLS):
            if (r,c) in coords:
                print(coords[(r,c)], end='')
            else:
                print('.', end='')
        print()

def move(robots:list[CoVe]):
    return [((r+vr) % ROWS, (c+vc) % COLS, vr,vc) for r,c,vr,vc in robots]

def count_quadrants(robots:list[CoVe]):
    quadrants = {1:0, 2:0, 3:0, 4:0}
    hrows, hcols = ROWS//2, COLS//2
    for r,c,_,_ in robots:
        if r < hrows and c < hcols:
            quadrants[1] += 1
        elif r < hrows and c > hcols:
            quadrants[2] += 1
        elif r > hrows and c < hcols:
            quadrants[3] += 1
        elif r > hrows and c > hcols:
            quadrants[4] += 1
        else:
            pass # the middle
    return quadrants

def part_one(robots:list[CoVe], steps:int=100):
    """Solution to part one."""
    for _ in range(steps):
        robots = move(robots)

    return math.prod(count_quadrants(robots).values())



def part_two(robots:list[CoVe]):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [tuple(map(int,PATTERN.findall(line.strip()))) for line in fileinput.input()]
    robots: list[CoVe] = [(r,c, vr,vc) for c,r, vc,vr in lines] # type: ignore

    print('part_one', part_one(robots)) 
    print('part_two', part_two(robots))


main()
