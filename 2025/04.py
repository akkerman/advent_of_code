"""2025 Day 4: Printing Department"""
import fileinput

Coord = tuple[int, int]
Grid = set[Coord]

def neighbors(c:Coord) -> Grid:
    """Return all 8 neighboring coordinates."""
    x,y = c
    return {(x+dx, y+dy) 
            for dx in (-1,0,1) for dy in (-1,0,1) 
            if dx or dy}

def mark_movable(grid:Grid) -> Grid:
    """Return set of movable coordinates."""
    return {
      c for c in grid
      if len(neighbors(c) & grid) < 4
    }

def part_one(grid:Grid) -> int:
    """Solution to part one."""
    return len(mark_movable(grid))

def part_two(grid:Grid) -> int:
    """Solution to part two."""
    current = set[Coord](grid)
    while movable := mark_movable(current):
        current -= movable
    return len(grid - current)

def main():
    """Parse input file, pass to puzzle solvers."""
    grid = Grid()

    for y,line in enumerate(fileinput.input()):
        for x,c in enumerate(line.strip()):
            if c == '@':
                grid.add((x,y))


    print('part_one', part_one(grid))
    print('part_two', part_two(grid))

if __name__ == '__main__':
    main()
