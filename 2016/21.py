"""Day 21: Scrambled Letters and Hash."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def part_one(instructions: list[str]):
    """Solution to part one."""
    passwd = list('abcdefgh')

    for instruction in instructions:
       if 'swap position' in instruction:
           res = re.match(r'swap position (\d+) with position (\d+)', instruction)
           assert res is not None
           x, y = res.groups()
           ix, iy = int(x), int(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue

       if 'swap letter' in instruction:
           res = re.match(r'swap letter (\w) with letter (\w)', instruction)
           assert res is not None
           x, y = res.groups()
           ix, iy = passwd.index(x), passwd.index(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue

       if 'rotate left' in instruction:
           res = re.match(r'rotate left (\d+) step', instruction)
           assert res is not None
           x = res.group(1)
           p = deque(passwd)
           p.rotate(-int(x))
           passwd = list(p)
           continue

       if 'rotate right' in instruction:
           res = re.match(r'rotate right (\d+) step', instruction)
           assert res is not None
           x = res.group(1)
           p = deque(passwd)
           p.rotate(int(x))
           passwd = list(p)
           continue

       if 'reverse positions' in instruction:
           res = re.match(r'reverse positions (\d+) through (\d+)', instruction)
           assert res is not None
           x, y = res.groups()
           ix, iy = int(x), int(y)
           sub = passwd[ix:iy+1]
           sub.reverse()
           passwd[ix:iy+1] = sub
           continue

       if 'move position' in instruction:
           res = re.match(r'move position (\d+) to position (\d+)', instruction)
           assert res is not None
           x, y = res.groups()
           ix, iy = int(x), int(y)
           char = passwd.pop(ix)
           passwd.insert(iy, char)
           continue
    
       if 'rotate based' in instruction:
           res = re.match(r'rotate based on position of letter (\w)', instruction)
           assert res is not None
           x = res.group(1)
           ix = passwd.index(x)
           steps = 1 + ix
           if ix >= 4:
               steps += 1
           p = deque(passwd)
           p.rotate(steps)
           passwd = list(p)
           continue


    
    
       print(f'Unknown instruction: {instruction}')


    return ''.join(passwd)


def part_two(instructions):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    instructions: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        instructions.append(line)

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()
