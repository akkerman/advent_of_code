# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

# brute force fill all the houses
def part_one(target):
    """ part one """
    m=target//10

    houses=[0]*m
    for elf in range(1, m):
        for nmbr in range(elf, m, elf):
            houses[nmbr] += elf * 10

    for nmbr, presents in enumerate(houses):
        if presents > target:
            return nmbr

    return None


def part_two(target):
    """ part two """
    m=target//10

    houses=[0]*m
    for elf in range(1, m):
        for nmbr in range(elf, m, elf):
            if nmbr // elf > 50:
                continue
            houses[nmbr] += elf * 11

    for nmbr, presents in enumerate(houses):
        if presents > target:
            return nmbr

    return None


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(int(line))

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))


main()
