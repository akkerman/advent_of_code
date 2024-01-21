# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import defaultdict
import re


re_cpy = re.compile('cpy (.+) (.+)')
re_inc = re.compile('inc (.+)')
re_dec = re.compile('dec (.+)')
re_jnz = re.compile('jnz (.+) (.+)')

def solve(lines, c = 0):
    registers = defaultdict(int)
    registers['c'] = c
    idx = 0
    while 0 <= idx < len(lines):
        line = lines[idx]

        if 'cpy' in line:
            m = re_cpy.match(line)
            assert m
            x, y = m.groups()
            try:
                x = int(x)
                registers[y] = x
            except ValueError:
                registers[y] = registers[x]
            idx += 1

        if 'inc' in line:
            m = re_inc.match(line)
            assert m
            x = m.groups()[0]
            registers[x] += 1
            idx += 1

        if 'dec' in line:
            m = re_dec.match(line)
            assert m
            x = m.groups()[0]
            registers[x] -= 1
            idx += 1

        if 'jnz' in line:
            m = re_jnz.match(line)
            assert m
            x, y = m.groups()
            try:
                value = int(x)
            except ValueError:
                value = registers[x]

            if value != 0:
                idx += int(y)
            else:
                idx += 1

    return registers['a']


def part_one(lines):
    """ part one """
    return solve(lines)


def part_two(lines):
    """ part two """
    return solve(lines, c=1)


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
