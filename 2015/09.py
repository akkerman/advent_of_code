# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from itertools import permutations

reg = re.compile(r'(.+) to (.+) = (\d+)')
def main():
    """ main """
    vertices=set()
    edges={}
    for line in sys.stdin:
        line = line.replace('\n', '')
        f, t, l = reg.match(line).groups()

        vertices.add(f)
        vertices.add(t)
        edges[(f,t)] = int(l)
        edges[(t,f)] = int(l)

    lengths = []
    for perm in permutations(vertices):
        lengths.append(sum(( edges[ft] for ft in zip(perm, perm[1:]) )))

    print('part_one', min(lengths))
    print('part_two', max(lengths))

main()
