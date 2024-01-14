# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from itertools import combinations
from functools import reduce
from operator import mul, add

def qa(group)->int:
    """ calculate the quantum entanglement of a group of package weights """
    return reduce(mul, group, 1)

def groups(package_weights):
    weight = sum(package_weights) // 3
    for len1 in range(2, len(package_weights) - 4):
        for g1 in combinations(package_weights, len1):
            if sum(g1) != weight:
                continue
            after = [p for p in package_weights if p not in g1]
            for len2 in range(2, len(after) - 2):
                for g2 in combinations(after, len2):
                    if sum(g2) != weight or len(g2) < len(g1):
                        continue
                    g3 = tuple(p for p in after if p not in g2)
                    if sum(g3) != weight or len(g3) < len(g1):
                        continue

                    return [g1, g2, g3]

    return []



def part_one(package_weights):
    """ part one """
    return qa(groups(package_weights)[0])


def part_two(package_weights):
    """ part two """
    return 'todo'


def main():
    """ main """
    package_weights = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = int(line)
        package_weights.append(line)

    print('part_one', part_one(package_weights))

    print('part_two', part_two(package_weights))


main()
