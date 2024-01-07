# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string
import re
from itertools import permutations


def part_one(vertices, edges):
    """ part one """
    all_happy = []
    for perm in permutations(vertices):
        z = zip(perm, perm[1:]+(perm[0],))
        happy = 0
        for entry in z:
            happy += edges[entry]
            happy += edges[entry[::-1]]
        all_happy.append(happy)

    return max(all_happy)

def part_two(vertices, edges):
    """ part two """
    for person in vertices:
        edges[(person, 'Akkerman')]=0
        edges[('Akkerman', person)]=0
    vertices.add('Akkerman')

    return part_one(vertices, edges)


def main():
    """ main """
    lines = []

    reg = re.compile(r'(.+) would (gain|lose) (\d+) .+to (.+)\.')

    vertices = set()
    edges = {}



    for line in sys.stdin:
        line = line.replace('\n', '')

        p1, gl, amount, p2 = reg.match(line).groups()

        amount = int(amount) if gl == 'gain' else -int(amount)

        vertices.add(p1)
        vertices.add(p2)
        edges[(p1,p2)] = amount


    print('part_one', part_one(vertices, edges))

    print('part_two', part_two(vertices, edges))


main()
