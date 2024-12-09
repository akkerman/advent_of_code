# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

DOT = -607

DEBUG = False

def print_diskmap(diskmap: List[int]):
    if not DEBUG:
        return
    for d in diskmap:
        if d == DOT:
            print('.', end='')
        else:
            print(d, end='')
    print()

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
            continue
        file_id = diskmap[i]
        assert isinstance(file_id, int)
        c += file_id * i
    return c


def part_one(diskmap: List[int]) -> int:
    """ part one """
    return checksum(move(explode(diskmap)))


def find_free_space(diskmap: List[int], length: int) -> int:
    sequence = [DOT] * length
    for i in range(len(diskmap) - length):
        if diskmap[i:i+length] == sequence:
            return i
    return -1

def move_2(diskmap: List[int]):
    file_end_idx = len(diskmap) - 1
    file_start_idx = file_end_idx

    print_diskmap(diskmap)
    while file_end_idx > 0:
        file_start_idx = diskmap.index(diskmap[file_end_idx])
        file = diskmap[file_start_idx:file_end_idx+1]

        free_start_idx = find_free_space(diskmap, len(file))
        if free_start_idx >= 0 and free_start_idx < file_start_idx:
            diskmap[free_start_idx:free_start_idx+len(file)] = file
            diskmap[file_start_idx:file_end_idx+1] = [DOT] * len(file)
            print_diskmap(diskmap)

        file_end_idx = file_start_idx - 1
        while diskmap[file_end_idx] == DOT:
            file_end_idx -= 1

    return diskmap




def part_two(diskmap: List[int]):
    """ part two """
    dm = move_2(explode(diskmap))
    print_diskmap(dm)
    return checksum(dm)


def main():
    """ main """
    diskmap: List[int] = []
    for line in sys.stdin:
        line = line.strip()
        diskmap += list(map(int, list(line)))

    print('part_one', part_one(diskmap))

    print('part_two', part_two(diskmap))


main()
