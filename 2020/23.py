# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def pick_three(cups, current):
    pos = cups.index(current)
    cs = cups+cups
    three = cs[pos+1:pos+4]
    return three

def select_destination(cups, current):
    c = int(current)
    for _ in range(1,10):
        c=(c-1)%10
        if str(c) in cups:
            return str(c)

    raise ValueError('no destination')

def place(cups, three, current, dest):
    pos = cups.index(dest) + 1
    posc = cups.index(current)
    if pos < posc:
        cs = cups[pos:] + cups[:pos]
        return place(cs, three, current, dest)

    cups[pos:pos] = three
    return cups

def new_current(cups, current):
    pos = cups.index(current)
    cs = cups + cups
    return cs[pos+1]

mv = 0
def move(cups, current):
    global mv
    mv += 1
    print(f"-- move {mv} --")
    three = pick_three(cups, current)
    print('cups:', " ".join([f'({c})' if c == current else c for c in cups]))
    print('pick up:', ", ".join(three))
    cs = [c for c in cups if c not in three]
    dest = select_destination(cs, current)
    print('destination:', dest)
    cs = place(cs, three, current, dest)

    return cs, new_current(cs, current)

def part_one(cups):
    """ part one """
    cs = list(cups)
    c = cups[0]
    for _ in range(0,100):
        cs, c = move(cs, c)

    pos = cs.index('1')

    return "".join(cs[pos+1:] + cs[:pos])


def part_two(cups):
    """ part two """
    return 'todo'


def main():
    """ main """
    cups=[]
    for line in sys.stdin:
        line = line.replace('\n', '')
        cups = line
        break


    print('part_one', part_one(cups))

    print('part_two', part_two(cups))


main()
