"""Day 24: Crossed Wires."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Gate = tuple[str, str, str, str]
def part_one(wires: dict[str, int], gates: list[Gate]) -> tuple[int, str]:
    """Solution to part one."""

    q = deque[Gate](gates)

    while q:
        gate = q.popleft()
        lhs, op, rhs, target = gate
        
        if lhs not in wires or rhs not in wires:
            q.append(gate)
            continue

        if op == 'AND':
            wires[target] = wires[lhs] & wires[rhs]
        elif op == 'OR':
            wires[target] = wires[lhs] | wires[rhs]
        elif op == 'XOR':
            wires[target] = wires[lhs] ^ wires[rhs]
        else:
            raise ValueError('Unknown operator')

    zwires = sorted([k for k in wires.keys() if k.startswith('z')], reverse=True)

    bin_output = ''.join([str(wires[w]) for w in zwires])

    return int(bin_output, 2), bin_output


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    wires: dict[str, int] = {}
    parsing = 'wires'
    gates: list[Gate] = []

    GATE_PATTERN = re.compile(r'(\w+) (AND|OR|XOR) (\w+) -> (\w+)')
    for line in fileinput.input():
        line = line.strip()
        if line == '':
            parsing = 'gates'
            continue

        if parsing == 'wires':
            wire, signal = line.split(': ')
            wires[wire] = int(signal)

        if parsing == 'gates':
            gates.append(tuple(GATE_PATTERN.findall(line)[0])) # type: ignore
        

    print('part_one', part_one(wires, gates))

    print('part_two', part_two(wires))


if __name__ == '__main__':
    main()
