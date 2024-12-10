"""Day 5: Print Queue."""
import sys
from util_05 import check_order, make_sort_pages

def middle_page(pages: list[int]) -> int:
    """Returns the number in the  middle of the list."""
    return pages[len(pages) // 2]

def part_one(rules: list[tuple[int, int]], updates: list[list[int]]) -> int:
    """Sum all middle pages that are in order."""
    return sum(middle_page(update) for update in updates if check_order(rules, update))

def part_two(rules: list[tuple[int, int]], updates: list[list[int]]) -> int:
    """Sum all middle pages after sorting when not in order."""
    sort_pages = make_sort_pages(rules)
    return sum(
        middle_page(update)
        for update in updates
        if not check_order(rules, update) and sort_pages(update)
    )


def main():
    """Parse input file, pass to puzzle solvers."""
    rules: list[tuple[int, int]] = []
    updates: list[list[int]] = []
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
