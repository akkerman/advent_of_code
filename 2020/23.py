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
    def __init__(self, value:str):
        self.value=value
        self.next=self
        self.prev=self

    def __repr__(self):
        return f"{self.prev.value}-({self.value})-{self.next.value}"

def part_two(cupValues):
    """ part two """

    maxValue = 1000000

    lookup = {}

    def init():
        cups = [Cup(v) for v in cupValues]

        first = cups[0]
        last = cups[-1]
        for i,c in enumerate(cups):
            lookup[c.value] = c
            if i > 0:
                c.prev = cups[i-1]
            if i < len(cups)-1:
                c.next = cups[i+1]

        if maxValue > 10:
            for i in range(10, maxValue+1):
                s = str(i)
                c = Cup(s)
                lookup[s] = c
                if i == 10:
                    continue
                prev = lookup[str(i-1)]
                c.prev = prev
                prev.next = c

            last.next = lookup['10']
            lookup['10'] = last
            last = lookup['1000000']

        first.prev = last
        last.next = first

        return first

    def pick_three(current:Cup)->Cup:
        t1 = current.next
        t2 = t1.next
        t3 = t2.next

        after = t3.next

        current.next = after
        after.prev = current

        t1.prev = t1
        t3.next = t3

        return t1

    def select_destination(current, three):
        vals = [three.value, three.next.value, three.next.next.value]
        i = int(current.value) - 1

        while True:
            if i <= 0:
                i = maxValue
            if str(i) in vals:
                i-=1
                continue

            return lookup[str(i)]

    def place(dest, three):
        t1 = three
        t2 = t1.next
        t3 = t2.next

        t3.next = dest.next
        t1.prev = dest
        dest.next = t1



    def move(current:Cup)->Cup:
        print("current:", current.value)
        print("cups    ", values_from(current))
        three = pick_three(current)

        print("pick up:", three.value, three.next.value, three.next.next.value)
        dest = select_destination(current, three)
        print('destination: ', dest.value)
        print("place before", values_from(current))
        place(dest, three)
        print("place after ", values_from(current))
        print("place after ", values_from(dest))
        print("")

        return current.next


    def values_from(current):
        c = current
        nums = []
        for _ in range(0,20):
            nums.append(c.value)
            c = c.next
        return " ".join(nums)



    current = init()


    # print(values_from(current))
    for _ in range(0, 10): # 10000000):
        current = move(current)

    return values_from(lookup['1'])


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
