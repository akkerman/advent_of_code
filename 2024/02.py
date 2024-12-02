# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

def is_save(report: List[int]) -> bool:
    if not sorted_or_reversed(report):
        return False

    for l1, l2 in zip(report, report[1:]):
        if not is_save_level(l1, l2):
            return False

    return True

def sorted_or_reversed(report: List[int]) -> bool:
    sorted_report = sorted(report)
    reversed_report = sorted(report, reverse=True)
    
    if report != sorted_report and report != list(reversed_report):
        return False 
    return True

def is_save_level(l1: int, l2: int) -> bool:
    diff = abs(l1 - l2)
    if diff == 0 or diff > 3:
        return False
    return True

def is_save_with_dampeners(report: List[int]) -> bool:
    if is_save(report):
        return True

    for i in range(0, len(report)):
        dampened = report[:i] + report[i+1:]
        if is_save(dampened):
            return True

    return False


def part_one(reports: List[List[int]]):
    """ part one """
    sum = 0
    for report in reports:
        if is_save(report):
            sum += 1
    return sum


def part_two(reports: List[List[int]]):
    """ part two """
    sum = 0
    for report in reports:
        if is_save_with_dampeners(report):
            sum += 1
    return sum


def main():
    """ main """
    reports: List[List[int]] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        report = [int(l) for l in line.split(' ')]
        
        reports.append(report)

    print('part_one', part_one(reports))

    print('part_two', part_two(reports))


main()
