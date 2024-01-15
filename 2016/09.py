# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

x = re.compile(r'(\d+)x(\d+)')

def decompress(line):
    f = line.index('(')
    t = line.index(')')
    m = x.match(line[f+1:t])
    assert m
    lng, reps = [int(n) for n in m.groups()]


    part = line[t+1:t+1+lng]
    part = part.replace('(', '_')
    part = part.replace(')', '-')
    part = part * reps
    line = line[:f] + part + line[t+1+lng:]

    return line




def part_one(compressed_file):
    """ part one """
    dec = compressed_file

    while '(' in dec:
        dec = decompress(dec)

    dec=dec.replace('_', '(')
    dec=dec.replace('-', ')')
    return len(dec)


def part_two(compressed_file):
    """ part two """
    return 'todo'


def main():
    """ main """
    compressed_file = ''
    for line in sys.stdin:
        line = line.replace('\n', '')
        compressed_file = line
        break

    print('part_one', part_one(compressed_file))

    print('part_two', part_two(compressed_file))


main()
