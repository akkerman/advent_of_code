# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import Counter

def part_one(box_ids):
    """ part one """
    x2 = 0
    x3 = 0
    for bid in box_ids:
        c = Counter(list(bid))
        if 2 in c.values():
            x2+=1
        if 3 in c.values():
            x3+=1
    return x2*x3


def part_two(box_ids):
    """ part two """
    for idx, xs in enumerate(box_ids):
        for idy in range(idx+1, len(box_ids)):
            ys=box_ids[idy]
            check = [i for i,(x,y) in enumerate(zip(xs, ys)) if x!=y]
            if len(check) == 1:
                i=check[0]
                return xs[0:i]+xs[i+1:]
    return 'error'


def main():
    """ main """
    box_ids = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        box_ids.append(line)

    print('part_one', part_one(box_ids))

    print('part_two', part_two(box_ids))


main()
