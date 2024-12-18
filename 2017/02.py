"""Day 2: Corruption Checksum."""
import sys
import re

def part_one(spreadsheet: list[list[int]]):
    """Sumarize the difference of the max and min of each row."""
    return sum(max(row) - min(row) for row in spreadsheet)


def part_two(spreadsheet: list[list[int]]):
    """Sumarize the division of the only two numbers that are divisible."""
    return sum(
        a // b 
        for row in spreadsheet 
        for a in row for b in row 
        if a != b and a % b == 0
    )
    


def main():
    """Parse input file, pass to puzzle solvers."""
    spreadsheet: list[list[int]] = []
    for line in sys.stdin:
        line = line.strip()
        line = list(map(int,re.findall(r'\d+', line)))
        
        spreadsheet.append(line)

    print('part_one', part_one(spreadsheet))

    print('part_two', part_two(spreadsheet))


main()
