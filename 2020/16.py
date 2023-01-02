# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
from collections import defaultdict
import sys
import string

from itertools import chain


def make_is_valid(fields):
    def is_valid(ticket):
        for num in ticket:
            match_any = False
            for ranges in fields.values():
                if num in ranges:
                    match_any = True
            if not match_any:
                return False
        return True
    return is_valid


def part_one(fields, tickets):
    """ part one """
    total = 0
    for ticket in tickets:
        for num in ticket:
            match_any = False
            for ranges in fields.values():
                if num in ranges:
                    match_any = True
            if not match_any:
                total += num

    return total


def part_two(fields, ticket, tickets):
    """ part two """
    is_valid = make_is_valid(fields)
    valid_tickets = [t for t in tickets if is_valid(t)]
    candidates = defaultdict(list)

    for field, ranges in fields.items():
        for i in range(len(ticket)):
            all_match = True
            for t in valid_tickets:
                if t[i] not in ranges:
                    all_match = False
            if all_match:
                candidates[field].append(i)

    while True:
        length_one = [lst[0] for lst in candidates.values() if len(lst) == 1]
        if len(length_one) == len(candidates):
            break
        for num in set(length_one):
            for c in candidates.values():
                if num in c and len(c) > 1:
                    c.remove(num)

    product = 1
    for field, value in candidates.items():
        if 'departure' in field:
            product *= ticket[value[0]]

    return product


def main():
    """ main """
    parsing = 'fields'

    fields = {}
    ticket = []
    tickets = []

    for line in sys.stdin:
        line = line.replace('\n', '')

        if line == '':
            continue

        if line == 'your ticket:':
            parsing = 'ticket'
            continue

        if line == 'nearby tickets:':
            parsing = 'tickets'
            continue

        match parsing:
            case 'fields':
                field, ranges = line.split(': ')
                ranges = ranges.split(' or ')
                start0, end0 = ranges[0].split('-')
                start1, end1 = ranges[1].split('-')

                fields[field] = list(chain(
                        range(int(start0), int(end0) + 1),
                        range(int(start1), int(end1) + 1)
                        ))

            case 'ticket':
                ticket = [int(n) for n in line.split(',')]
            case 'tickets':
                t = [int(n) for n in line.split(',')]
                tickets.append(t)


    print('part_one', part_one(fields, tickets))

    print('part_two', part_two(fields, ticket, tickets))



main()
