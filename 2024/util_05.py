from typing import List, Tuple

def get_rules(rules: List[Tuple[int,int]], left:int) -> List[Tuple[int, int]]:
    return [(l,r) for l,r in rules if l == left]

def check_order(rules: List[Tuple[int, int]], pages: List[int]) -> bool:
    for i in range(len(pages)):
        current_page = pages[i]
        for _, r in get_rules(rules, current_page):
            if r in pages and not r in pages[i+1:]:
                return False
    return True

