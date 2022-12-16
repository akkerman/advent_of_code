import sys

import functools

def compare(left, right):
    if isinstance(left, int) and isinstance(right, int):
        return left - right

    if isinstance(left, list) and isinstance(right, list):
        for l, r in zip(left, right):
            result = compare(l, r)
            if result:
                return result

        return len(left) - len(right)

    # exactly one value is integer
    if isinstance(left, int):
        return compare([left], right)

    return compare(left, [right])


def part_one(pairs):
    idxs = [i for i, (left, right) in enumerate(pairs, 1) if compare(eval(left), eval(right)) < 0]

    return sum(idxs)  # expect 5659

def part_two(pairs):
    divider2 = [[2]]
    divider6 = [[6]]

    packets = [divider2, divider6]
    for left, right in pairs:
        packets.append(eval(left))
        packets.append(eval(right))

    packets.sort(key=functools.cmp_to_key(compare))

    return (1 + packets.index(divider2)) * (1 + packets.index(divider6))


def main():
    lines = []
    left = None
    right = None
    for line in sys.stdin:
        line = line.replace('\n', '')
        if line == '':
            lines.append(( left, right ))
            left = None
            right = None
            continue

        if not left:
            left = line
            continue

        if not right:
            right = line
            continue

    if left and right:
        lines.append((left,right))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
