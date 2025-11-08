"""Day 25: Clock Signal."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer


re_cpy = re.compile('cpy (.+) (.+)')
re_inc = re.compile('inc (.+)')
re_dec = re.compile('dec (.+)')
re_jnz = re.compile('jnz (.+) (.+)')
re_tgl = re.compile('tgl (.+)')
re_out = re.compile('out (.+)')

def solve(lines: list[str], a: int) -> int:
    idx = 0
    registers: dict[str,int] = defaultdict(int)
    registers['a'] = a
    
    expected_output = 0

    while 0 <= idx < len(lines):
        line = lines[idx]
    
        if (m := re_cpy.match(line)):
            x, y = m.groups()
            try:
                x = int(x)
                registers[y] = x
            except ValueError:
                registers[y] = registers[x]
            idx += 1
            continue

        if (m := re_inc.match(line)):
            x = m.groups()[0]
            registers[x] += 1
            idx += 1
            continue

        if (m := re_dec.match(line)):
            x = m.groups()[0]
            registers[x] -= 1
            idx += 1
            continue

        if (m := re_jnz.match(line)):
            x, y = m.groups()
            try:
                value = int(x)
            except ValueError:
                value = registers[x]

            if value != 0:
                try:
                    idx += int(y)
                except ValueError:
                    idx += registers[y]
            else:
                idx += 1
            continue

        if (m := re_out.match(line)):
            x = m.groups()[0]
            try:
                value = int(x)
            except ValueError:
                value = registers[x]

            if value != expected_output:
                return -1  # signal failure
            expected_output = 1 - expected_output  

            idx += 1
            continue

        if (m := re_tgl.match(line)):
            x = m.groups()[0]
            try:
                offset = int(x)
            except ValueError:
                offset = registers[x]

            target_idx = idx + offset
            if 0 <= target_idx < len(lines):
                instr = lines[target_idx]
                if 'cpy' in instr:
                    lines[target_idx] = instr.replace('cpy', 'jnz', 1)
                elif 'jnz' in instr:
                    lines[target_idx] = instr.replace('jnz', 'cpy', 1)
                elif 'inc' in instr:
                    lines[target_idx] = instr.replace('inc', 'dec', 1)
                elif 'dec' in instr:
                    lines[target_idx] = instr.replace('dec', 'inc', 1)
                elif 'tgl' in instr:
                    lines[target_idx] = instr.replace('tgl', 'inc', 1)

                print(f'toggled instruction at {target_idx} from "{instr}" to "{lines[target_idx]}"')
            
            idx += 1
            continue

    return registers['a']

def part_one(lines):
    """Solution to part one."""
    a = 4
    while True:
        a += 1
        print(f'trying a={a}')
        result = solve(lines, a)


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    lines = [
        line.strip()
        for line in fileinput.input()
        ]

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()
