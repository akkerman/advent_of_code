# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

import regex as re


expected_fields = {'byr', 'iyr', 'eyr', 'hcl', 'ecl', 'pid', 'cid', 'hgt'}
optional_fields = {'cid'}


def passport_fields(line):
    fields = set()
    for field in line.strip().split(' '):
        fields.add(field.split(':')[0])
    return fields

def has_valid_fields(fields): 
    return fields == expected_fields or (expected_fields - fields) == optional_fields


def as_passport(line):
    passport = {}
    for field in line.strip().split(' '):
        key, value = field.split(':')
        passport[key] = value
    return passport


def is_valid_passport(passport):
    byr = int(passport['byr'])
    if not 1920 <= byr <= 2002:
        return False

    iyr = int(passport['iyr'])
    if not 2010 <= iyr <= 2020:
        return False

    eyr = int(passport['eyr'])
    if not 2020 <= eyr <= 2030:
        return False

    hgt = passport['hgt']
    if re.match(r'^\d+in$', hgt):
        hgt = int(hgt[0:-2])
        if not 59 <= hgt <= 76:
            return False
    elif re.match(r'^\d+cm$', hgt):
        hgt = int(hgt[0:-2])
        if not 150 <= hgt <= 193:
            return False
    else:
        return False

    hcl = passport['hcl']
    if not re.match(r'^#[0-9a-f]{6}$', hcl):
        return False

    if passport['ecl'] not in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']:
        return False

    pid = passport['pid']
    if not re.match(r'^[0-9]{9}$', pid):
        return False

    return True


def part_one(lines):
    valid = 0
    for line in lines:
        if has_valid_fields(passport_fields(line)):
            valid += 1

    return valid


def part_two(lines):
    valid = 0
    for line in lines:
        if not has_valid_fields(passport_fields(line)):
            continue

        passport = as_passport(line)
        if not is_valid_passport(passport):
            continue

        valid += 1

    return valid


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
