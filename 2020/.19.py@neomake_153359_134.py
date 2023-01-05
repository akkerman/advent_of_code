# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def make_matcher(rules):
    def match(msg, rule=rules['0']):
        if msg == '' and rule == []:
            return True
        if msg == '' or rule == []:
            return False

        head = rule[0]
        tail = rule[1:]
        sub = rules[head]

        if type(sub) == str:
            return sub == msg[0] and match(msg[1:], tail)

        if type(sub) == list:
            if '|' in sub:
                idx = sub.index('|')
                if match(msg, sub[:idx] + tail):
                    return True
                return match(msg, sub[idx+1:] + tail)

            return match(msg, sub + tail)

        assert False
    return match


def part_one(rules, messages):
    """ part one """
    match = make_matcher(rules)

    total = 0
    for msg in messages:
        if match(msg):
            total += 1
    return total


def part_two(rules, messages):
    """ part two """

    rules['8'] = '42 | 42 8'.split(' ')
    rules['11'] = '42 31 | 42 11 31'.split(' ')

    match = make_matcher(rules)

    total = 0
    for msg in messages:
        if match(msg):
            print(msg)
            total += 1
    return total


def main():
    """ main """
    rules = dict()
    messages = []
    parse_messages = False
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            parse_messages = True
            continue

        if parse_messages:
            messages.append(line)
        else:
            k,v = line.split(': ')
            if '"' in v:
                v = v.replace('"', '')
            else:
                v = v.split(' ')

            rules[k] = v


    print('part_one', part_one(rules, messages))

    print('part_two', part_two(rules, messages))

main()
