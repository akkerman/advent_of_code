"""2017 Day 14: Disk Defragmentation"""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import chunk_list, perf_timer
from operator import xor
from functools import reduce



def knot_hash(input: str) -> str:
    lst = list(range(256))
    return to_hex(dense_hash(sparse_hash(lst, translate_length(input))))

def to_hex(lst: list[int]):
    return "".join([f"{i:02x}" for i in lst])

def sparse_hash(lst: list[int], lengths: list[int], iterations: int = 64):
    pos = 0
    skip = 0
    n = len(lst)

    for _ in range(iterations):
        for lng in lengths:
            reverse_sub(lst, pos, lng)
            pos = (pos + lng + skip) % n
            skip += 1

    return lst

length_suffix = [17, 31, 73, 47, 23]
def translate_length(lengths: str):
    return [ord(s) for s in lengths] + length_suffix

def reverse_sub(lst: list[int], start:int, length:int):
    n = len(lst)
    for i in range(length // 2):
        s = (start + i) % n
        e = (start + length - 1 - i) % n
        lst[s], lst[e] = lst[e], lst[s]

def dense_hash(lst: list[int]):
    dense: list[int] = []
    for block in chunk_list(lst, 16):
        dense.append(reduce(xor,block))
    return dense

def to_bin(s: str):
    return "".join(f"{int(c,16):04b}" for c in s)

def part_one(key:str):
    """Solution to part one."""
    count = 0
    for i in range(128):
        hash = knot_hash(f"{key}-{i}")
        bin_str = to_bin(hash)
        count += bin_str.count('1')
    return count

dirs = [(-1,0),(1,0),(0,-1),(0,1)]


def part_two(key:str):
    """Solution to part two."""
    coords_with_ones: set[tuple[int,int]] = set()
    for i in range(128):
        hash = knot_hash(f"{key}-{i}")
        bin_str = to_bin(hash)
        for j, c in enumerate(bin_str):
            if c == '1':
                coords_with_ones.add((i,j))

    regions = 0

    while coords_with_ones:
        start = coords_with_ones.pop()

        queue = deque([start])

        visited: set[tuple[int,int]] = set()
        visited.add(start)

        while queue:
            x,y = queue.popleft()
            neighbors = [
                coord for dx,dy in dirs
                if (coord:=(x+dx, y+dy)) in coords_with_ones
                    ]

            for n in neighbors:
                visited.add(n)
                queue.append(n)
                coords_with_ones.remove(n)


        regions += 1

    return regions


def main():
    """Parse input file, pass to puzzle solvers."""
    key: str = next(fileinput.input()).strip() 


    print('part_one', part_one(key))

    print('part_two', part_two(key))


if __name__ == '__main__':
    main()
