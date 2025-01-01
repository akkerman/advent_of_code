"""Day 5: Print Queue."""
import sys
from functools import cmp_to_key

def get_page_ordering_rules(rules: list[tuple[int,int]], left:int):
    """Return all ordering rules for a given page."""
    return ((l,r) for l,r in rules if l == left)

def check_order(rules: list[tuple[int, int]], pages: list[int]) -> bool:
    """Check if the pages respect the ordering rules."""
    for i, current_page in enumerate(pages):
        ordering_rules = get_page_ordering_rules(rules, current_page)

        def violates_order(right_page: int) -> bool:
            return right_page in pages and right_page not in pages[i+1:]

        if any(violates_order(r) for _,r in ordering_rules):
            return False

    return True

def make_sort_pages(rules: list[tuple[int, int]]):
    """Create a sort function based on the list of pairs."""
    def compare(x:int, y:int) -> int:
        return -1 if (x,y) in rules else 0
    def sort_pages(update: list[int]):
        update.sort(key=cmp_to_key(compare))
        return update
    return sort_pages

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

if __name__ == '__main__':
    main()

rules = [
      (47,53), (97,13), (97,61), (97,47), (75,29), (61,13), (75,53),
      (29,13), (97,29), (53,29), (61,53), (97,53), (61,29), (47,13),
      (75,47), (97,75), (47,61), (75,61), (47,29), (75,13), (53,13),
]

def test_get_rules():
   assert list(get_page_ordering_rules(rules, 97)) == [(97,13), (97,61), (97,47), (97,29), (97,53), (97,75)]
   assert list(get_page_ordering_rules(rules, 75)) == [(75,29), (75,53), (75,47), (75,61), (75,13)]
   assert list(get_page_ordering_rules(rules, 29)) == [(29,13)]
   assert list(get_page_ordering_rules(rules, 13)) == []

def test_check_order():
   assert check_order(rules, [75,47,61,53,29]) == True
   assert check_order(rules, [97,61,53,29,13]) == True
   assert check_order(rules, [75,29,13]) == True
   assert check_order(rules, [75,97,47,61,53]) == False
   assert check_order(rules, [61,13,29]) == False
   assert check_order(rules, [97,13,75,29,47]) == False

def test_sort_pages():
   sort_pages = make_sort_pages(rules)
   assert sort_pages([75,97,47,61,53]) == [97,75,47,61,53]
   assert sort_pages([61,13,29]) == [61,29,13]
   assert sort_pages([97,13,75,29,47]) == [97,75,47,29,13]
