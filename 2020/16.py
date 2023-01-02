# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

from itertools import chain

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
    return 'todo'


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
    # too high 50234380


main()
