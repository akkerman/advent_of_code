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


def decompress_v2(line):
    if not '(' in line:
        return len(line)

    f = line.index('(')
    t = line.index(')')
    m = x.match(line[f+1:t])
    assert m
    lng, reps = [int(n) for n in m.groups()]

    part = line[t+1:t+1+lng]

    init = f # all chars before first (
    marked = reps * decompress_v2(part)
    rest = decompress_v2(line[t+1+lng:])

    return init + marked + rest


def part_two(compressed_file):
    """ part two """
    return decompress_v2(compressed_file)


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
