import sys
from typing import DefaultDict

diff = {
    'D': (0, 1),
    'U': (0, -1),
    'L': (-1, 0),
    'R': (1, 0),
}

def solve(lines, length):
    rope = [(0, 0)] * length
    tailpositions = {(0, 0)}

    for direction, steps in lines:
        for _ in range(steps):
            head0, head1 = rope[0]
            diff0, diff1 = diff[direction]
            rope[0] = head0+diff0, head1+diff1

            for knot in range(length-1):
                head0, head1 = rope[knot]
                tail0, tail1 = rope[knot+1]

                if tail0 == head0 and tail1 < head1 - 1:
                    tail1 = head1 - 1
                elif tail0 == head0 and tail1 > head1 + 1:
                    tail1 = head1 + 1
                elif tail1 == head1 and tail0 < head0 - 1:
                    tail0 = head0 - 1
                elif tail1 == head1 and tail0 > head0 + 1:
                    tail0 = head0 + 1
                elif tail0 != head0 and tail1 != head1:
                    diff_lr = head0 - tail0
                    diff_ud = head1 - tail1

                    if abs(diff_lr) > 1 or abs(diff_ud) > 1:
                        tail0 += 1 if diff_lr > 0 else -1
                        tail1 += 1 if diff_ud > 0 else -1

                rope[knot+1] = tail0, tail1

            tailpositions.add(rope[-1])

    return len(tailpositions)


def main():
    lines = []
    for line in sys.stdin:
        direction, steps = line.split(' ')
        lines.append((direction, int(steps)))

    print('part_one', solve(lines, 2))

    print('part_two', solve(lines, 10))


main()
