from typing import List, Tuple
from functools import cmp_to_key

def get_rules(rules: List[Tuple[int,int]], left:int) -> List[Tuple[int, int]]:
    return [(l,r) for l,r in rules if l == left]

def check_order(rules: List[Tuple[int, int]], pages: List[int]) -> bool:
    for i in range(len(pages)):
        current_page = pages[i]
        for _, r in get_rules(rules, current_page):
            if r in pages and not r in pages[i+1:]:
                return False
    return True

def make_sort_pages(rules: List[Tuple[int, int]]):
    def compare(x:int, y:int) -> int:
        return -1 if (x,y) in rules else 0
    def sort_pages(update: List[int]):
        update.sort(key=cmp_to_key(compare))
        return update
    return sort_pages
