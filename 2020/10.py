# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import defaultdict


def part_one(adapters):
    """ part one """
    outlet = 0
    device = max(adapters) + 3
    chain = [outlet] + sorted(adapters) + [device]
    diffs = defaultdict(int)

    for i in range(len(chain) - 1):
        diff = chain[i+1] - chain[i]
        diffs[diff] += 1

    assert len(diffs) == 2
    return diffs[1] * diffs[3]


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(int(line))

    print('part_one', part_one(lines))
    print('part_two', part_two(lines))


main()
