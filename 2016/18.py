"""Day 18: Like a Rogue."""
import fileinput
from collections import Counter

traps = [ '^^.', '.^^', '^..', '..^' ]


def next_row(current_row:str) -> str:
    """Generate the next row based on current row."""
    nr = ''
    width = len(current_row)
    for i in range(width):
        if i == 0:
            fragment = '.' + current_row[i:i+2]
        elif i == width - 1:
            fragment = current_row[i-1:i+1] + '.'
        else:
            fragment = current_row[i-1:i+2]

        nr += '^' if fragment in traps else '.'

    return nr

def solve(row:str, total_rows:int) -> int:
    nr = row
    cnt = Counter(nr)
    for _ in range(total_rows - 1):
        nr = next_row(nr)
        cnt.update(nr)
    return cnt['.']

def main():
    """Parse input file, pass to puzzle solvers."""
    lines:list[str] = []
    for line in fileinput.input():
        line = line.strip()
        
        lines.append(line)

    print('part_one', solve(lines[0], 40))

    print('part_two', solve(lines[0], 400000))


if __name__ == '__main__':
    main()
