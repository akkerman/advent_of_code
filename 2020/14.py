# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def part_one(lines):
    """ part one """
    mask = ''
    mem = {}
    for what, value in lines:
        if what == 'mask':
            mask = value
            continue

        mem[what] = overwrite_int(mask, value)

    return sum(mem.values())


def part_two(lines):
    """ part two """

    mask = ''
    mem = {}
    for what, value in lines:
        if what == 'mask':
            mask = value
            continue

        address = int_to_bits(what.replace('mem[', '').replace(']', ''))
        address = overwrite_x(mask, address)

        for addr in generate_addresses(address):
            mem[addr] = int(value)

    return sum(mem.values())


def int_to_bits(num):
    return f"{int(num):36b}".replace(' ', '0')


def bits_to_int(bits):
    return int(bits, 2)


def overwrite(mask, value):
    result = ""
    for m, v in zip(mask, value):
        if m == 'X':
            result += v
        else:
            result += m

    return result


def overwrite_x(mask, value):
    result = ""
    for m, v in zip(mask, value):
        if m == '0':
            result += v
        elif m == '1':
            result += '1'
        elif m == 'X':
            result += 'X'
        else:
            assert False

    return result


def overwrite_int(mask, value):
    return bits_to_int(overwrite(mask, int_to_bits(value)))


def generate_addresses(address):
    if 'X' not in address:
        yield address
    else:
        for x0 in generate_addresses(address.replace('X', '0', 1)):
            yield x0
        for x1 in generate_addresses(address.replace('X', '1', 1)):
            yield x1


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '').split(' = ')
        lines.append(line)

    print('part_one', part_one(lines))


    print('part_two', part_two(lines))

main()
