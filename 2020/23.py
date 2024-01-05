# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys

def part_one(cups):
    """ part one """

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

    def move(cups, current):
        three = pick_three(cups, current)
        cs = [c for c in cups if c not in three]
        dest = select_destination(cs, current)
        cs = place(cs, three, current, dest)

        return cs, new_current(cs, current)


    cs = list(cups)
    c = cups[0]
    for _ in range(0,100):
        cs, c = move(cs, c)

    pos = cs.index('1')

    return "".join(cs[pos+1:] + cs[:pos])


class Cup:
    """ representation of a Cup """
    def __init__(self, value:int):
        self.value=value
        self.next=self

    def __repr__(self):
        return f"({self.value})-{self.next.value}"

def part_two(cupValues):
    """ part two """

    ONE_MILLION = 1000000
    TEN_MILLION = 10000000

    maxValue = ONE_MILLION

    lookup = {}
    nums = set()
    nums.update(cupValues)

    def init()->Cup:
        """ Create circle of 1M cups, return first """
        cups = [Cup(int(v)) for v in cupValues]

        first = cups[0]
        last = cups[-1]
        for i,c in enumerate(cups):
            lookup[c.value] = c
            if i < len(cups)-1:
                c.next = cups[i+1]

        if maxValue > 10:
            for i in range(10, maxValue+1):
                nums.add(i)
                c = Cup(i)
                lookup[i] = c
                if i == 10:
                    continue
                lookup[i-1].next = c

            lookup[int(cupValues[-1])].next = lookup[10]
            last = lookup[1000000]

        last.next = first

        return first

    def pickup_three(current:Cup)->Cup:
        """ pick three Cups up from out of the circle
            modifying the circle, returning the first of the 
            picked up cups """

        t1 = current.next
        t2 = t1.next
        t3 = t2.next

        after = t3.next

        current.next = after
        t3.next = t3

        return t1

    def select_destination(current:Cup, three:Cup)->Cup:
        """ Select the destination to place the, previously picked up cups
            wil not select one of the three cups
            """
        vals = [three.value, three.next.value, three.next.next.value]
        i = current.value - 1

        while True:
            if i <= 0:
                i = maxValue
            if i in vals:
                i-=1
                continue

            return lookup[i]

    def place(dest:Cup, three:Cup)->None:
        """ Place the three cups after the destination """
        t1 = three
        t2 = t1.next
        t3 = t2.next
        t3.next = dest.next
        dest.next = t1

    def move(current:Cup)->Cup:
        """ execute one move i.e:
            - pick three cups that are clockwise to current
            - select destination cup
            - place three cups clockwise of destination
            - select new current cup
            """
        three = pickup_three(current)
        dest = select_destination(current, three)
        place(dest, three)
        return current.next

    ########
    current = init()
    if maxValue > 10:
        for _ in range(0, TEN_MILLION):
            current = move(current)
    else:
        for _ in range(0, 100):
            current = move(current)

    one = lookup[1]
    return one.next.value * one.next.next.value


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
