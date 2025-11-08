"""Day 24: Air Duct Spelunking."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int, int]

def part_one(lines: list[str]):
    """Solution to part one."""
    maze, start = parse_maze(lines)
    to_visit_num = frozenset(set(maze.values()) - {'.'})

    queue: list[tuple[int, Coord, frozenset[str]]] = []
    visited: set[tuple[Coord, frozenset[str]]] = set()
    heapq.heappush(queue, (0, start, frozenset({'0'})))

    offsets = [(-1,0),(1,0),(0,-1),(0,1)]

    while queue:
        steps, position, visited_num = heapq.heappop(queue)
        if visited_num == to_visit_num:
            return steps
        state = (position, visited_num)
        if state in visited:
            continue
        visited.add(state)

        x, y = position

        for neighbor in [n for dx, dy in offsets if (n:=(x+dx, y+dy)) in maze]:
            char = maze[neighbor]
            if char in to_visit_num:
                new_visited_num = frozenset(visited_num | {char})
            else:
                new_visited_num = visited_num
            heapq.heappush(queue, (steps+1, neighbor, new_visited_num))

    assert False, "No solution found"



def part_two(lines):
    """Solution to part two."""
    return 'todo'

def parse_maze(lines: list[str]) -> tuple[dict[Coord, str], Coord]:
    """Parse maze into graph representation."""
    maze:dict[Coord, str] = {}
    start: Coord = (0,0)
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char == '#':
                continue
            if char == '0':
                start = (x, y)
            maze[(x, y)] = char
    return maze, start

def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [line.strip() for line in fileinput.input()]

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
