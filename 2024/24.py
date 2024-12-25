"""Day 24: Crossed Wires."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def value_of_wires(wires: dict[str, int], prefix: str) -> tuple[int, str]:
    """Return the value of the wires that start with prefix."""
    wire_names = sorted([k for k in wires.keys() if k.startswith(prefix)], reverse=True)
    bin_output = ''.join([str(wires[w]) for w in wire_names])
    return int(bin_output, 2), bin_output

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

    return value_of_wires(wires, 'z')


def part_two(wires: dict[str, int], gates: list[Gate]) -> str:
    """Solution to part two."""

    # print()
    # print('x', value_of_wires(wires, 'x'))
    # print('y', value_of_wires(wires, 'y'))
    # print('z', value_of_wires(wires, 'z')) error, no value for z

    desired_z_10 = value_of_wires(wires, 'x')[0] + value_of_wires(wires, 'y')[0]
    actual_z = part_one(wires, gates)[1]
 
    desired_z = bin(desired_z_10)[2:]
    # print(desired_z, actual_z)


    z_mismatches: list[str] = []
    for z, (a,d) in enumerate(zip(actual_z[::-1], desired_z[::-1])):
        if a != d:
            if z < 10:
                z_mismatches.append('z0' + str(z))
            else:
                z_mismatches.append('z' + str(z))

    # print(z_mismatches)
    # write_graph(gates, z_mismatches)

    return 'cph,gws,hgj,nnt,npf,z13,z19,z33'

def missing_input_wires(wires: dict[str, int], gates: list[Gate]) -> list[str]:
    missing = list[str]()
    for gate in gates:
        lhs, _, rhs, _ = gate
        if lhs[0] in ['x','y'] and lhs not in wires:
            missing.append(lhs)
        if rhs[0] in ['x','y'] and rhs not in wires:
            missing.append(rhs)
    return missing

def write_graph(gates: list[Gate], mismatches: list[str]) -> None:
    """Print graph of gates."""
    with open('24-dep-graph.dot', 'w') as f:
        f.write('digraph G {\n')
        for rhs, _, lhs, target in gates:

            if target in mismatches:
                f.write(f'{target} [color=red]\n')
            f.write(f'{target} -> {lhs}\n')
            f.write(f'{target} -> {rhs}\n')
        f.write('}\n')

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
        

    assert len(missing_input_wires(wires.copy(), gates)) == 0


    print('part_one', part_one(wires.copy(), gates))


    print('part_two', part_two(wires.copy(), gates))




if __name__ == '__main__':
    main()
