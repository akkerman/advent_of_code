""" Day 4: Ceres Search """
import sys

def rotate(grid: list[str]) -> list[str]:
    """Rotate the grid 90 degrees."""
    return [''.join([grid[j][i] for j in range(len(grid))]) for i in range(len(grid[0]))]

def count_xmas(grid: list[str]) -> int:
    """Count XMAS and SAMX in each line."""
    count = 0
    for line in grid:
        count += line.count('XMAS')
        count += line.count('SAMX')
    return count

def main_diagonals(grid: list[str]) -> list[str]:
    """Extract main diagonals from the grid."""
    rows = len(grid)
    cols = len(grid[0])

    diagonals: list[str] = []
    for d in range(rows + cols - 1):
        dia = ''
        for r in range(d + 1):
            c = d - r
            if r < rows and c < cols:
                dia += grid[r][c]
        diagonals.append(dia)
    return diagonals

def anti_diagonals(grid: list[str]) -> list[str]:
    """Extract anti diagonals from the grid."""
    rows = len(grid)
    cols = len(grid[0])

    diagonals: list[str] = []
    for d in range(rows + cols - 1):
        dia = ''
        for r in range(d + 1):
            c = d - r
            if r < rows and c < cols:
                dia += grid[r][cols - c - 1]
        diagonals.append(dia)
    return diagonals


def part_one(grid: list[str]):
    """Count the number of times XMAS appears."""
    count = 0
    count += count_xmas(grid)
    count += count_xmas(rotate(grid))
    count += count_xmas(main_diagonals(grid))
    count += count_xmas(anti_diagonals(grid))
    return count

def is_xmas(grid: list[str], r: int, c: int) -> bool:
    topleft = grid[r-1][c-1]
    topright = grid[r-1][c+1]
    bottomleft = grid[r+1][c-1]
    bottomright = grid[r+1][c+1]

    main = (topleft == 'M' and bottomright == 'S') or (topleft == 'S' and bottomright == 'M')
    anti = (topright == 'M' and bottomleft == 'S') or (topright == 'S' and bottomleft == 'M')

    return main and anti

def part_two(grid: list[str]) -> int:
    """Count the number of times X-MAS appears."""
    rows = len(grid)
    cols = len(grid[0])
    count = 0
    for r in range(1,rows-1):
        for c in range(1,cols-1):
            if grid[r][c] == 'A':
                if is_xmas(grid, r, c):
                    count += 1
    return count


def main():
    """ main """
    grid: list[str] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        grid.append(line)

    print('part_one', part_one(grid))

    print('part_two', part_two(grid))


main()
