# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

def part_one(changes):
    """ part one """
    return sum(changes)

def part_two(changes):
    """ part two """
    seen = set()
    f = 0
    idx = 0
    while f not in seen:
        seen.add(f)
        f += changes[idx]
        idx = (idx + 1) % len(changes)
    return f

def main():
    """ main """
    changes = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        line = int(line)
        changes.append(line)

    print('part_one', part_one(changes))

    print('part_two', part_two(changes))


main()
