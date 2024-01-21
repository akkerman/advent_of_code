# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque


class Office:
    def __init__(self, favo):
        self.favo = favo

    def is_open(self, x, y):
        a = x*x + 3*x + 2*x*y + y + y*y + self.favo
        return bin(a).count("1") % 2 == 0

    def is_ok(self, x, y):
        if x < 0 or y < 0:
            return False
        return self.is_open(x, y)

    def neighbours(self, x, y):
        xs = [(x-1, y), (x+1, y), (x, y-1), (x,y+1)]
        return [(x,y) for x,y in xs if self.is_ok(x,y)]


def part_one(favo, end):
    """ part one """
    office = Office(favo)

    q = deque()
    q.append((1,1,0))
    visited = set()

    while True:
        x,y,steps = q.popleft()
        if (x,y) in visited:
            continue
        if (x,y) == end:
            return steps

        visited.add((x,y))

        for nx, ny in office.neighbours(x,y):
            q.append((nx,ny,steps+1))



def part_two(favo):
    """ part two """
    office = Office(favo)

    q = deque()
    q.append((1,1,0))
    visited = set()

    while True:
        if len(q) == 0:
            break

        x,y,steps = q.popleft()
        if (x,y) in visited:
            continue
        if steps > 50:
            continue

        visited.add((x,y))

        for nx, ny in office.neighbours(x,y):
            q.append((nx,ny,steps+1))

    return len(visited)

def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    favo = int(lines[0])
    x,y = lines[1].split(',')
    x=int(x)
    y=int(y)
    print('part_one', part_one(favo, end=(x,y)))

    print('part_two', part_two(favo))


main()
