"""Day 19: An Elephant Named Joseph."""
import fileinput
from collections import deque

def part_one(count:int):
    """Solution to part one."""
    elfs = deque(range(1, count + 1))
    while len(elfs) > 1:
        elfs.append(elfs.popleft())
        elfs.popleft()

    return elfs.popleft()


def part_two(count:int):
    """Solution to part two."""
    left = deque(range(1, count // 2 + 1))
    right = deque(range(count // 2 + 1, count + 1))

    while left and right:
        if len(left) > len(right):
            left.pop()
        else:
            right.popleft()
        right.append(left.popleft())
        left.append(right.popleft())

    return left[0] if left else right[0]


def main():
    """Parse input file, pass to puzzle solvers."""
    count = 0
    for line in fileinput.input():
        line = line.strip()
        count = int(line)
        break
        

    print('part_one', part_one(count))

    print('part_two', part_two(count))


if __name__ == '__main__':
    main()
