# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

def part_one(row, col):
    """ part one """
    r = 1
    c = 1
    max_row = 1
    code = 20151125

    while True:
        if r == row and c == col:
            return code

        code *= 252533
        code %= 33554393

        if r > 1:
            c += 1
            r -= 1
        else:
            max_row += 1
            r = max_row
            c = 1

def main():
    """ main """
    row = 0
    col = 0
    for line in sys.stdin:
        line = line.replace('\n', '')
        m = re.match(r'.+row (\d+), column (\d+).', line)
        assert m
        r,c = m.groups()
        row = int(r)
        col = int(c)
        break

    print('part_one', part_one(row, col))



main()
