"""Day 17: Two Steps Forward."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

import hashlib

Coord = tuple[int, int]
Dir = str

def is_open(c:str)->bool:
    return c in 'bcdef'

def md5(s:str)->str:
    m = hashlib.md5()
    m.update(s.encode())
    return m.hexdigest()

def door_status(passwd:str)->tuple[bool,bool,bool,bool]:
    """Return door status as (up, down, left, right)."""
    hex = md5(passwd)

    up = is_open(hex[0])
    down = is_open(hex[1])
    left = is_open(hex[2])
    right = is_open(hex[3])
    
    return (up, down, left, right)

def neighbors(pos:Coord, passwd:str)->list[tuple[Coord, Dir]]:
    """Return list of valid neighbor positions."""
    x, y = pos
    
    coords: list[Coord] = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
    possibilities = zip (coords, 'UDLR', door_status(passwd) )
    nbs: list[tuple[Coord,Dir]] = []

    for (nx, ny), dir, is_open in possibilities:
        if is_open and 1 <= nx <= 4 and 1 <= ny <= 4:
            nbs.append(((nx, ny), dir))
    
    return nbs

def part_one(passwd:str) -> str:
    """Solution to part one."""
    start: Coord = (1, 1)
    q: list[tuple[int, Coord, str]] = [(0, start, '')]
    
    while q:
        steps, pos, path = heapq.heappop(q)

        if pos == (4,4):
            return path

        for npos, dir in neighbors(pos, passwd+path):
            heapq.heappush(q, (steps+1, npos, path + dir))

    return ''

def part_two(passwd:str):
    """Solution to part two."""

    @lru_cache(None)
    def longest(bla: tuple[int, Coord, str]) -> int: 
        steps, pos, path = bla
        if pos == (4,4):
            return len(path)

        lenpath = -1
        for npos, dir in neighbors(pos, passwd+path):
            plen = longest((steps+1, npos, path + dir))
            if plen > lenpath:
                lenpath = plen
        return lenpath


    return longest((0, (1, 1), ''))


def main():
    """Parse input file, pass to puzzle solvers."""
    lines: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        lines.append(line)

    password = lines[0]

    print('part_one', part_one(password))

    print('part_two', part_two(password))


if __name__ == '__main__':
    main()


def test_part_one1():
    """Test part one."""
    assert(part_one('ihgpwlah')) == 'DDRRRD'
def test_part_one2():
    """Test part one."""
    assert(part_one('kglvqrro')) == 'DDUDRLRRUDRD'
def test_part_one3():
    """Test part one."""
    assert(part_one('ulqzkmiv')) == 'DRURDRUDDLLDLUURRDULRLDUUDDDRR'

def test_part_two1():
    """Test part two."""
    assert(part_two('ihgpwlah')) == 370
def test_part_two2():
    """Test part two."""
    assert(part_two('kglvqrro')) == 492
def test_part_two3():
    """Test part two."""
    assert(part_two('ulqzkmiv')) == 830
