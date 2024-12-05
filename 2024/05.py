# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Tuple
from util_05 import check_order, make_sort_pages

def middle_page(pages: List[int]) -> int:
    idx = int(len(pages)/2)
    return pages[idx]

def part_one(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ part one """
    sum = 0
    for update in updates:
        if check_order(rules, update):
            sum += middle_page(update)
    return sum

def part_two(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ part two """
    sort_pages = make_sort_pages(rules)
    sum = 0
    for update in updates:
        if not check_order(rules, update):
            sort_pages(update)
            sum += middle_page(update)
    return sum

def main():
    """ main """
    rules: List[Tuple[int, int]] = []
    updates: List[List[int]] = []

    processing = 'ordering'
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            processing = 'updates'
            continue

        if processing == 'ordering':
            parts = line.split('|')
            rules.append((int(parts[0]), int(parts[1])))
            continue

        if processing == 'updates': 
            line = line.split(',')
            updates.append([int(x) for x in line])
        

    print('part_one', part_one(rules, updates))

    print('part_two', part_two(rules, updates))

main()
