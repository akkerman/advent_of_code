# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

def part_one(lines):
    """ part one """
    def num_space(n):
        if n in '-0123456789':
            return n
        return ' '
    nums = "".join([num_space(n) for n in lines[0]])
    nums = re.split(r' +', nums)

    return sum(int(n) for n in nums if n != '')

def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    # too high: 126674
    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
