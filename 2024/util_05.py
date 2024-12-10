"""Utility functions for the fifth aoc puzzle."""
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
