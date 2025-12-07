"""2018 Day 11: Chronal Charge"""
import fileinput
from collections import defaultdict

Coord = tuple[int, int]

GRID_SIZE = 300

def calculate_power_level(x:int, y:int, grid_serial_number:int) -> int:
    rack_id = x + 10
    power_level = rack_id * y
    power_level += grid_serial_number
    power_level *= rack_id
    power_level = (power_level // 100) % 10
    power_level -= 5
    return power_level

def calculate_power_grid(grid_serial_number:int) -> dict[Coord,int]:
    """Calculate power levels for each cell in the grid."""
    power_grid = dict[Coord,int]()
    for x in range(1, GRID_SIZE + 1):
        for y in range(1, GRID_SIZE + 1):
            power_grid[(x, y)] = calculate_power_level(x, y, grid_serial_number)
    return power_grid

def part_one(grid_serial_number:int):
    """Solution to part one."""
    power_grid = calculate_power_grid(grid_serial_number)

    max_power = float('-inf')
    top_left_coord = (0, 0)
    for x in range(1, 299):
        for y in range(1, 299):
            total_power = sum(
                power_grid[(x + dx, y + dy)]
                for dx in range(3)
                for dy in range(3)
            )
            if total_power > max_power:
                max_power = total_power
                top_left_coord = (x, y)
    return f"{top_left_coord[0]},{top_left_coord[1]}"

def summed_area_table(power_grid:dict[Coord,int]) -> dict[Coord,int]:
    """ Calculate summed area table for the power grid."""
    # See https://en.wikipedia.org/wiki/Summed-area_table
    sat = defaultdict[Coord,int](int)
    for x in range(1, GRID_SIZE + 1):
        for y in range(1, GRID_SIZE + 1):
            sat[(x, y)] = (
                power_grid[(x, y)]
                + sat.get((x - 1, y), 0)
                + sat.get((x, y - 1), 0)
                - sat.get((x - 1, y - 1), 0)
            )
    return sat

def calculate_top_left(sat:dict[Coord,int], square_size:int):
    """Calculate top-left coordinate of square with given size with max power."""
    max_power = float('-inf')
    top_left_coord = (0, 0)
    max_top_left = GRID_SIZE - square_size + 2
    for x in range(1, max_top_left):
        for y in range(1, max_top_left):
            x2 = x + square_size - 1
            y2 = y + square_size - 1
            total_power = (
                sat[(x2, y2)]
                - sat[(x - 1, y2)]
                - sat[(x2, y - 1)]
                + sat[(x - 1, y - 1)]
            )
            if total_power > max_power:
                max_power = total_power
                top_left_coord = (x, y)

    return top_left_coord, max_power


def part_two(grid_serial_number:int):
    """Solution to part two."""
    power_grid = calculate_power_grid(grid_serial_number)
    sat = summed_area_table(power_grid)
    max_power = float('-inf')
    best_size = 0
    best_coord = (0, 0)
    for square_size in range(GRID_SIZE+1):
        top_left, power = calculate_top_left(sat, square_size)
        if power > max_power:
            max_power = power
            best_coord = top_left
            best_size = square_size
    return f"{best_coord[0]},{best_coord[1]},{best_size}"



def main():
    """Parse input file, pass to puzzle solvers."""

    grid_serial_number: int = int(next(fileinput.input()).strip())

    print('part_one', part_one(grid_serial_number))

    print('part_two', part_two(grid_serial_number))


if __name__ == '__main__':
    main()

def test_calculate_power_level_1():
    assert calculate_power_level(3, 5, 8) == 4
def test_calculate_power_level_2():
    assert calculate_power_level(122, 79, 57) == -5
def test_calculate_power_level_3():
    assert calculate_power_level(217, 196, 39) == 0
def test_calculate_power_level_4():
    assert calculate_power_level(101, 153, 71) == 4
