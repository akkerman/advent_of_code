# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys


def generate_0(rules):
    def gen(rule):
        if rule == []:
            yield rule
            return

        head = rule[0]
        tail = rule[1:]
        sub = rules[head]

        if type(sub) == list:
            if '|' in sub:
                idx = sub.index('|')
                for r in gen(sub[:idx] + tail):
                    yield r
                for r in gen(sub[idx+1:] + tail):
                    yield r
            else:
                for r in gen(sub + tail):
                    yield r
        elif sub in 'ab':
            for r in gen(tail):
                yield [sub] + r

    return gen(rules['0'])


def part_one(rules, messages):
    """ part one """
    rule0 = []
    for rule in generate_0(rules):
        rule0.append(''.join(rule))

    total = 0
    for msg in messages:
        if msg in rule0:
            total += 1

    return total


def part_two(lines):
    """ part two """
    return 'todo'


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

    print('part_two', part_two(rules))


main()
