# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Tuple
from util_05 import check_order, make_sort_pages

def middle_page(pages: List[int]) -> int:
    return pages[len(pages) // 2]

def part_one(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ sum all middle pages that are in order """
    return sum(middle_page(update) for update in updates if check_order(rules, update))

def part_two(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ sum all middle pages after sorting when not in order """
    sort_pages = make_sort_pages(rules)
    return sum(
        middle_page(update)
        for update in updates
        if not check_order(rules, update) and sort_pages(update)
    )


def main():
    """ main """
    rules: List[Tuple[int, int]] = []
    updates: List[List[int]] = []
    processing = 'ordering'
    
    for line in sys.stdin:
        line = line.strip()
        if not line:
            processing = 'updates'
            continue

        if processing == 'ordering':
            rule = tuple(map(int, line.split('|')))
            assert len(rule) == 2
            rules.append(rule)
        else:  # processing == 'updates'
            updates.append(list(map(int, line.split(','))))

        

    print('part_one', part_one(rules, updates))

    print('part_two', part_two(rules, updates))

main()
