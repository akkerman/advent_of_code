"""2017 Day 21: Fractal Art"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer
from typing import Generator

Grid = list[str]
start: Grid = [
        '.#.', 
        '..#', 
        '###'
        ]

def rotate(grid: Grid) -> Grid:
    """Rotate pattern 90 degrees clockwise."""
    return ["".join(row) for row in zip(*grid[::-1])]

def flip(grid: Grid) -> Grid:
    """Flip pattern horizontally."""
    return [row[::-1] for row in grid]

def grid_permutations(grid: Grid) -> Generator[Grid, None, None]:
    """Get all unique transformations (rotations and flips) of a grid."""
    current = grid
    for _ in range(4):
        yield current
        yield flip(current)
        current = rotate(current)

def grid_to_str(grid: Grid) -> str:
    """Convert grid to string representation."""
    return "/".join(grid)

def str_to_grid(s: str) -> Grid:
    """Convert string representation to grid."""
    return s.split("/")


def transform(grid: Grid, rules: dict[str, str]) -> Grid:
    """Transform the grid according to the rules."""
    for perm in grid_permutations(grid):
        perm_str = grid_to_str(perm)
        if perm_str in rules:
            return str_to_grid(rules[perm_str])
    assert False, "No matching rule found"

def split_grid(grid:Grid) -> list[Grid]:
    """Split a larger grid into smaller grids."""
    grid_size = len(grid)
    square_size = 2 if grid_size % 2 == 0 else 3
    squares: list[Grid] = []
    for row in range(0, grid_size, square_size):
        for col in range(0, grid_size, square_size):
            square: Grid = []
            for r in range(square_size):
                square.append(grid[row + r][col:col + square_size])
            squares.append(square)
    return squares

def join_grids(grids: list[Grid]) -> Grid:
    """Join smaller grids into a larger grid."""
    num_grids = len(grids)
    grid_size = int(num_grids ** 0.5)
    square_size = len(grids[0])
    new_grid: Grid = []
    for row_block in range(grid_size):
        for r in range(square_size):
            new_row = ""
            for col_block in range(grid_size):
                new_row += grids[row_block * grid_size + col_block][r]
            new_grid.append(new_row)
    return new_grid


def part_one(rules:dict[str,str]):
    """Solution to part one."""
    grid = start
    for _ in range(5):
        split = split_grid(grid)
        transformed = [transform(g, rules) for g in split]
        grid = join_grids(transformed)
    return sum(row.count('#') for row in grid)

def part_two(rules):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    rules:dict[str,str] = {}
    for line in fileinput.input():
        f,t = line.strip().split(' => ')
        rules[f]=t

    print(transform(start, rules))

    print('part_one', part_one(rules))

    print('part_two', part_two(rules))


if __name__ == '__main__':
    main()

def test_split_grid_2_2():
    grid: Grid = ['12', '34']
    assert split_grid(grid) == [grid]

def test_split_grid_3_3():
    grid: Grid = ['123', '456', '789']
    assert split_grid(grid) == [grid]

def test_split_grid_4_2x2():
    input: Grid = ['#..#', '....', '....', '#..#']

    expected = [
            ['#.','..'],
            ['.#','..'],
            ['..','#.'],
            ['..','.#'],
            ]

    assert split_grid(input) == expected

def test_split_grid_6_3x3():
    input: Grid = [
            '##.##.',
            '#..#..',
            '......',
            '##.##.',
            '#..#..',
            '......',
            ]

    expected = [
            ['##','#.'],
            ['.#','.#'],
            ['#.','..'],
            ['..','##'],
            ['..','.#'],
            ['..','#.'],
            ['#.','..'],
            ['.#','..'],
            ['..','..'],
            ]

    assert split_grid(input) == expected


def test_join_grid_2_2():
    grid: Grid = ['12', '34']
    assert join_grids([grid]) == grid

def test_join_grid_3_3():
    grid: Grid = ['123', '456', '789']
    assert join_grids([grid]) == grid


def test_join_grids_3x3_6():
    expected: Grid = [
            '##.##.',
            '#..#..',
            '......',
            '##.##.',
            '#..#..',
            '......',
            ]

    grids: list[Grid] = [
            ['##','#.'],
            ['.#','.#'],
            ['#.','..'],
            ['..','##'],
            ['..','.#'],
            ['..','#.'],
            ['#.','..'],
            ['.#','..'],
            ['..','..'],
            ]

    assert join_grids(grids) == expected





