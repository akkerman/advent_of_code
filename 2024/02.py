"""Day 2: Red-Nosed Reports."""
import sys
from typing import List

def is_save(report: List[int]) -> bool:
    """Check if the report is safe."""
    if not is_monotonic(report):
        return False

    for l1, l2 in zip(report, report[1:]):
        if not is_save_level(l1, l2):
            return False

    return True

def is_monotonic(report: List[int]) -> bool:
    """Check if the levels are all decreasing or all increasing."""
    return (
        all(report[i] <= report[i+1] for i in range(0, len(report)-1)) or
        all(report[i] >= report[i+1] for i in range(0, len(report)-1))
    )


def is_save_level(l1: int, l2: int) -> bool:
    """Check if the difference between levels is 1, 2 or 3."""
    return 1 <= abs(l1 - l2) <= 3

def is_save_with_dampener(report: list[int]) -> bool:
    """Check if the report is safe or can be made safe by removing one level."""
    return (
       is_save(report) or 
       any(is_save(report[:i] + report[i+1:]) for i in range(len(report)))
     )


def part_one(reports: List[List[int]]):
    """Sum the number of safe reports."""
    return sum(is_save(report) for report in reports)


def part_two(reports: List[List[int]]):
    """Sum the number of reports that can be made safe by removing one level."""
    return sum(is_save_with_dampener(report) for report in reports)


def main():
    """ main """
    reports: List[List[int]] = []
    for line in sys.stdin:
        line = line.strip()
        report = list(map(int, line.split(' ')))
        
        reports.append(report)

    print('part_one', part_one(reports))

    print('part_two', part_two(reports))


main()
