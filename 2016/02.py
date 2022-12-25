# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

keypad = [
    ['1', '2', '3'],
    ['4', '5', '6'],
    ['7', '8', '9'],
]

def part_one(lines):
    """ part one """
    code = ''
    r, c = 0, 0
    for line in lines:
        for d in line:
            if d == 'U' and r > 0:
                r -= 1
            if d == 'D' and r < 2:
                r += 1
            if d == 'R' and c < 2:
                c += 1
            if d == 'L' and c > 0:
                c -= 1
        code += keypad[r][c]
    return code


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
