# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
from functools import lru_cache
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


def part_two(adapters):
    """ part two """
    neighbours = {}
    for adapter in sorted([0] + adapters):
        neighbours[adapter] = sorted([a for a in adapters if adapter < a <= adapter+3])

    lst = max(adapters)
    cache = {}

    def dfs(start):
        if start == lst:
            return 1

        arrangements = 0
        for nb in neighbours[start]:
            if nb in cache:
                arrangements += cache[nb]
            else:
                result = dfs(nb)
                arrangements += result
                cache[nb] = result
        return arrangements

    return dfs(0)


def part_two_alt(adapters):
    neighbours = {}
    for adapter in [0] + adapters:
        neighbours[adapter] = [a for a in adapters if adapter < a <= adapter+3]

    lst = max(adapters)

    @lru_cache
    def dfs(start):
        if start == lst:
            return 1

        arrangements = 0
        for nb in neighbours[start]:
            result = dfs(nb)
            arrangements += result
        return arrangements

    return dfs(0)


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(int(line))

    print('part_one', part_one(lines))
    print('part_two', part_two(lines))
    print('part_alt', part_two_alt(lines))


main()