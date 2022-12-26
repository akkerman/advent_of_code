# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


expected_fields = set([
    'byr',
    'iyr',
    'eyr',
    'hgt',
    'hcl',
    'ecl',
    'pid',
    'cid',
    ])

optional_fields = {'cid'}


def passport_fields(line):
    fields = set()
    for field in line.strip().split(' '):
        fields.add(field.split(':')[0])
    return fields


def part_one(lines): 
    valid = 0
    for line in lines:
        fields = passport_fields(line)

        if (fields == expected_fields or (expected_fields - fields) == optional_fields):
            valid += 1

    return valid


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    part = ''
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            lines.append(part)
            part = ''
            continue
        part = part + ' ' + line

    lines.append(part)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
