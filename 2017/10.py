"""Day 10: Knot Hash."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import reduce
from operator import xor

def reverse_sub(lst: list[int], start:int, length:int):
    n = len(lst)
    for i in range(length // 2):
        s = (start + i) % n
        e = (start + length - 1 - i) % n
        lst[s], lst[e] = lst[e], lst[s]


def part_one(lst: list[int], lengths: list[int]):
    """Solution to part one."""
    pos = 0
    skip = 0
    lst = lst.copy()
    n = len(lst)

    for lng in lengths:
        reverse_sub(lst, pos, lng)
        pos = (pos + lng + skip) % n
        skip += 1

    return lst[0] * lst[1]

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

def dense_hash(lst: list[int]):
    dense: list[int] = []
    for i in range(len(lst)//16):
        idx = i * 16
        dense.append(reduce(xor,lst[idx:idx+16]))
    return dense

def to_hex(lst: list[int]):
    return "".join([f"{i:02x}" for i in lst])

length_suffix = [17, 31, 73, 47, 23]

def translate_length(lengths: str):
    return [ord(s) for s in lengths] + length_suffix

def part_two(lst: list[int], lengths: str):
    """Solution to part two."""
    
    return to_hex(dense_hash(sparse_hash(lst, translate_length(lengths))))



def main():
    """Parse input file, pass to puzzle solvers."""
    lengths: list[int] = []
    line: str = ''
    for line in fileinput.input():
        line = line.strip()
        lengths = list(map(int, line.split(',')))
        break

    print('part_one', part_one(list(range(256)), lengths))

    print('part_two', part_two(list(range(256)), line))


if __name__ == '__main__':
    main()
    
def test_part_one():
    """Test part one."""
    assert part_one([0,1,2,3,4],[3, 4, 1, 5]) == 12

def test_translate_length():
    assert translate_length('1,2,3') == [49,44,50,44,51,17,31,73,47,23]

def test_dense_hash():
    assert dense_hash([65,27,9,1,4,3,40,50,91,7,6,0,2,5,68,22]) == [64]

def test_to_hex():
    assert to_hex([64, 7, 255]) == '4007ff'

def test_part_two_empty():
   assert part_two(list(range(256)), '') == 'a2582a3a0e66e6e86e3812dcb672a272'
def test_part_two_aoc():
   assert part_two(list(range(256)), 'AoC 2017') == '33efeb34ea91902bb2f59c9920caa6cd'
def test_part_two_3():
   assert part_two(list(range(256)), '1,2,3') == '3efbe78a8d82f29979031a4aa0b16a9d'
def test_part_two_4():
   assert part_two(list(range(256)), '1,2,4') == '63960835bcdc130f0b66d7ff4f6a5a8e'



