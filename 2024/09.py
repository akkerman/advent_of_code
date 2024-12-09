# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

DOT = -607

def explode(diskmap: List[int]):
    """ explode """
    newdiskmap: List[int] = []
    id = 0
    for i in range(len(diskmap)):
        if i % 2 == 0: # file
            newdiskmap += [id] * diskmap[i]
            id += 1
        else: 
            newdiskmap += [DOT] * diskmap[i]
    return newdiskmap

def move(diskmap: List[int]):
    empty_idx = diskmap.index(DOT)
    file_idx = len(diskmap) - 1

    while file_idx > empty_idx:
        file = diskmap[file_idx]
        diskmap[empty_idx] = file
        diskmap[file_idx] = DOT
        while diskmap[empty_idx] != DOT:
            empty_idx += 1
        while diskmap[file_idx] == DOT:
            file_idx -= 1
    return diskmap

def checksum(diskmap: List[int]) -> int:
    c = 0
    for i in range(len(diskmap)):
        if diskmap[i] == DOT:
            break
        file_id = diskmap[i]
        assert isinstance(file_id, int)
        c += file_id * i
    return c


def part_one(diskmap: List[int]) -> int:
    """ part one """
    return checksum(move(explode(diskmap)))


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    diskmap: List[int] = []
    for line in sys.stdin:
        line = line.strip()
        diskmap += list(map(int, list(line)))

    print('part_one', part_one(diskmap))

    print('part_two', part_two(diskmap))


main()
