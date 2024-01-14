# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import Counter


def solve(lines, most=True):
    """ part one """
    rotated = []
    for _ in range(len(lines[0])):
        rotated.append([])
    for line in lines:
        for i,c in enumerate(line):
            rotated[i].append(c)

    word=""
    for r in rotated:
        c = Counter(r)
        if most:
            word += c.most_common()[0][0]
        else:
            word += list(reversed(c.most_common()))[0][0]


    return word




def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', solve(lines))

    print('part_two', solve(lines, most=False))

main()
