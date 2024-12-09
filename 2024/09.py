import sys

DOT = -607

DEBUG = False

def print_diskmap(diskmap: list[int]):
    if not DEBUG:
        return
    for d in diskmap:
        if d == DOT:
            print('.', end='')
        else:
            print(d, end='')
    print()

def explode(diskmap: list[int]) -> list[int]:
    """Translate dense diskmap to individual blocks."""
    newdiskmap: list[int] = []

    for index, count in enumerate(diskmap):
        block = [index // 2] * count if index % 2 == 0 else [DOT] * count
        newdiskmap.extend(block)

    return newdiskmap

def move(diskmap: list[int]):
    """ move (part of) files empty space on the left """
    empty_idx = diskmap.index(DOT)
    file_idx = len(diskmap) - 1

    while file_idx > empty_idx:
        file = diskmap[file_idx]
        diskmap[empty_idx] = file
        diskmap[file_idx] = DOT

        # find next empty space on the left
        while diskmap[empty_idx] != DOT:
            empty_idx += 1

        # find next file on the right
        while diskmap[file_idx] == DOT:
            file_idx -= 1
    return diskmap

def checksum(diskmap: list[int]) -> int:
    """ sum the product of file_id and position """
    return sum(file_id * i for i, file_id in enumerate(diskmap) if file_id != DOT)


def part_one(diskmap: list[int]) -> int:
    """ calculate checksum after moving (part of) files """
    return checksum(move(explode(diskmap)))


def find_free_space(diskmap: list[int], length: int) -> int:
    """ find continuous free spece of given length """
    sequence = [DOT] * length
    for i in range(len(diskmap) - length):
        if diskmap[i:i+length] == sequence:
            return i
    return -1

def move_2(diskmap: list[int]):
    """ move whole files to empty space on the left """
    file_end_idx = len(diskmap) - 1
    file_start_idx = file_end_idx

    print_diskmap(diskmap)
    while file_end_idx > 0:
        file_start_idx = diskmap.index(diskmap[file_end_idx])
        file = diskmap[file_start_idx:file_end_idx+1]

        free_start_idx = find_free_space(diskmap[:file_start_idx+1], len(file))
        if free_start_idx >= 0:
            diskmap[free_start_idx:free_start_idx+len(file)] = file
            diskmap[file_start_idx:file_end_idx+1] = [DOT] * len(file)
            print_diskmap(diskmap)

        file_end_idx = file_start_idx - 1
        while diskmap[file_end_idx] == DOT:
            file_end_idx -= 1

    return diskmap




def part_two(diskmap: list[int]):
    """ calculate checksum after moving files """
    dm = move_2(explode(diskmap))
    print_diskmap(dm)
    return checksum(dm)


def main():
    """ main """
    diskmap: list[int] = []
    for line in sys.stdin:
        line = line.strip()
        diskmap += list(map(int, list(line)))

    print('part_one', part_one(diskmap))

    print('part_two', part_two(diskmap))

main()
