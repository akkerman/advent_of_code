import sys
import string


def prio(char):
    return 1 + string.ascii_letters.index(char)


def part_one(lines):
    result = 0
    for line in lines:
        half = len(line) // 2
        item, = set(line[:half]) & set(line[half:])
        result += prio(item)

    return result


def part_two(lines):
    result = 0
    for i in range(0, len(lines), 3):
        item, = set(lines[i]) & set(lines[i+1]) & set(lines[i+2])
        result += prio(item)

    return result


def main():
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
