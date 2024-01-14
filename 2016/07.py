# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re

def has_abba(s):
    for i in range(len(s) - 3):
        if s[i] != s[i+1] and s[i] == s[i+3] and s[i+1] == s[i+2]:
            return True

    return False

def supports_tls(ip):
    # parts in square brackets alternate with parts outside them
    # idx 0 is outside
    # tls is supported if one or more of the even parts have abba and
    # none of the odd parts

    supported = False
    for i, part in enumerate(ip):
        if i % 2 == 0:
            supported |= has_abba(part)
            continue

        if has_abba(part):
            return False

    return supported



# too high: 151
def part_one(ips):
    """ part one """
    with_tls = [ip for ip in ips if supports_tls(ip)]
    print(with_tls)
    return len(with_tls)


def part_two(ips):
    """ part two """
    return 'todo'



def main():
    """ main """
    ips = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        ip =  re.split(r'\[|\]', line)
        ips.append(ip)

    print('part_one', part_one(ips))

    print('part_two', part_two(ips))


main()
