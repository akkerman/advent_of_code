# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List, Tuple
from util_05 import get_rules, check_order
from functools import cmp_to_key


def part_one(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ part one """
    sum = 0
    for update in updates:
        if check_order(rules, update):
            idx = int(len(update)/2)
            sum += update[idx]

    return sum

def part_two(rules: List[Tuple[int, int]], updates: List[List[int]]) -> int:
    """ part two """

    def compare(x:int, y:int) -> int:
        if (x,y) in rules:
            return -1
        else:
            return 0
        
    sum = 0
    for update in updates:
        if not check_order(rules, update):
            update.sort(key=cmp_to_key(compare))
            idx = int(len(update)/2)
            sum += update[idx]
            
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
