# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque


def next_positions(row, col, start, end):  # "neighbours"
    pos = []
    # print('new pos for ', row, col)
    for dr, dc in [(0, 1), (0, -1), (-1, 0), (1, 0), (0, 0)]:
        r = row + dr
        c = col + dc

        next_pos = (r, c)

        if next_pos in [start, end]:
            pos.append(next_pos)
        elif 0 <= r < maxRow and 0 <= c < maxCol:
            pos.append(next_pos)

    return pos


offset_directions = {  # opposite of the blizzards
        '>': (0, -1),
        '<': (0, 1),
        '^': (1, 0),
        'v': (-1, 0),
        }


def dodge_blizzards(blizzards, start, end, starttime):
    row, col = start
    time = starttime

    q = deque([(row, col, time)])
    visited = set()

    while q:
        row, col, time = q.popleft()
        if (row, col) == end:
            return time

        if (row, col, time) in visited:
            continue

        visited.add((row, col, time))

        time += 1
        for r, c in next_positions(row, col, start, end):
            if (r, c) in [start, end]:
                q.append((r, c, time))  # there are not blizzards
            for direction in '<>^v':
                dr, dc = offset_directions[direction]
                offsetR = (r + dr * time) % maxRow
                offsetC = (c + dc * time) % maxCol
                if (offsetR, offsetC) in blizzards[direction]:
                    break
            else:
                # valid next position (loop dit not break)
                q.append((r, c, time))

    print('oeps')
    sys.exit(1)


def part_one(blizzards, start, end):
    return dodge_blizzards(blizzards, start, end, 0)


def part_two(blizzards, start, end):
    time = 0
    time = dodge_blizzards(blizzards, start, end, time)
    time = dodge_blizzards(blizzards, end, start, time)
    time = dodge_blizzards(blizzards, start, end, time)

    return time


def create_blizzards(lines):
    blizzards = {
            '>': set(),
            '<': set(),
            '^': set(),
            'v': set(),
            }

    # discard outside walls to enable modulo wrapping
    for row, line in enumerate(lines):
        for col, char in enumerate(line):
            if char not in '<>^v':
                continue
            blizzards[char].add((row, col))

    return blizzards

# blizzards wrap around and overlap
# instead of creating one blizzard field
# create 4 fields we can check and move independently

# checking if a point is in the blizzard at time 4
# is the same as checking if a point, offset 4 positions,
# is in the blizzard at time 0


maxRow = None
maxCol = None


def main():
    global maxRow, maxCol

    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')[1:]
        lines.append(line)

    lines.pop(0)

    maxRow = len(lines) - 1
    maxCol = len(lines[0]) - 1

    start = (-1, 0)
    end = (maxRow, maxCol - 1)

    print('part_one', part_one(
        create_blizzards(lines),
        start, end
        ))

    print('part_two', part_two(
        create_blizzards(lines),
        start, end
        ))


main()
