# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from json import loads

def count(struct):
    if isinstance(struct,int):
        return struct
    if isinstance(struct,list):
        return sum((count(struct) for struct in struct))
    if not isinstance(struct,dict):
        return 0
    if 'red' in struct.values():
        return 0
    return count(list(struct.values()))


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
    return count(loads(lines[0]))

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
