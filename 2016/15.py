"""Day 15: Timing is Everything."""
import fileinput
import re
import itertools

def part_one(disks: list[list[int]]) -> int:
    """Solution to part one."""
    def get_pos(btn_press:int, disk:list[int]) -> int:
       btn_offset, num_pos, _, pos_offset = disk
       return (btn_press + btn_offset + pos_offset) % num_pos

    for btn_press in itertools.count():
        if any(get_pos(btn_press, disk) for disk in disks):
            continue
        return btn_press

    return -1

def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    disks: list[list[int]] = []
    for line in fileinput.input():
        line = line.strip()
        line = list(map(int, re.findall(r'\d+', line)))
        
        disks.append(line)

    print('part_one', part_one(disks))

    print('part_two', part_two(disks))


if __name__ == '__main__':
    main()
