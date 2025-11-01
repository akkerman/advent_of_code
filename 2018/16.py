"""Day 16: Chronal Classification."""
import fileinput
import re
from typing import Callable

OpcodeFn = Callable[[list[int], int, int], int]

opcodes: dict[str, OpcodeFn] = {
   'addr': lambda r, a, b: r[a] + r[b],
   'addi': lambda r, a, b: r[a] + b,
   'mulr': lambda r, a, b: r[a] * r[b],
   'muli': lambda r, a, b: r[a] * b,
   'banr': lambda r, a, b: r[a] & r[b],
   'bani': lambda r, a, b: r[a] & b,
   'borr': lambda r, a, b: r[a] | r[b],
   'bori': lambda r, a, b: r[a] | b,
   'setr': lambda r, a, _: r[a],
   'seti': lambda r, a, _: a,
   'gtir': lambda r, a, b: 1 if a > r[b] else 0,
   'gtri': lambda r, a, b: 1 if r[a] > b else 0,
   'gtrr': lambda r, a, b: 1 if r[a] > r[b] else 0,
   'eqir': lambda r, a, b: 1 if a == r[b] else 0,
   'eqri': lambda r, a, b: 1 if r[a] == b else 0,
   'eqrr': lambda r, a, b: 1 if r[a] == r[b] else 0,   
}

class Entry:
    """Log entry."""

    def __init__(self):
        """Initialize log entry."""
        self.before: list[int] = []
        self.instruction: list[int] = []
        self.after: list[int] = []

    def __repr__(self):
        """String representation."""
        return f'Before: {self.before}, Instruction: {self.instruction}, After: {self.after}'

def part_one(log: list[Entry]):
    """Solution to part one."""
    matches: list[list[str]] = []
    for entry in log:
        log_matches: list[str] = []
        for name, opcode in opcodes.items():
            registers = entry.before.copy()
            _, a, b, c = entry.instruction
            registers[c] = opcode(registers, a, b)
            if registers == entry.after:
                log_matches.append(name)
        matches.append(log_matches)

    return sum(1 for m in matches if len(m) >= 3), matches



def part_two(log: list[Entry], program: list[list[int]], matches: list[list[str]]):
    """Solution to part two."""
    assert len(log) == len(matches)


    mapping: dict[int, str] = dict()

    # search mapping between opcode id and name by
    # saving the mapping if there is 1 and
    # removing known mappings from previous matches
    # repeat until all mappings found

    while len(mapping) < len(opcodes):
        for entry, match in zip(log, matches):
            op_id = entry.instruction[0]

            if len(match) == 1: 
                if op_id not in mapping:
                    mapping[op_id] = match[0]

            match[:] = [x for x in match if x not in mapping.values()]


    # run the program
    r = [0,0,0,0]
    for instr in program:
        op_id, a, b, c = instr
        r[c] = opcodes[mapping[op_id]](r, a, b)

    return r

def main():
    """Parse input file, pass to puzzle solvers."""
    log: list[Entry] = []
    entry: Entry = Entry()
    parse_mode = 'logentries'
    program: list[list[int]] = []
    empties = 0
    for line in fileinput.input():
        line = line.strip()

        if parse_mode == 'logentries':
            if not line:
                empties += 1
                if empties == 2:
                    parse_mode = 'program'
            else:
                empties = 0
            if line.startswith('Before'):
                entry.before = list(map(int, re.findall(r'\d+', line)))
            elif line.startswith('After'):
                entry.after = list(map(int, re.findall(r'\d+', line)))
                log.append(entry)
                entry = Entry()
            elif line:
                entry.instruction = list(map(int, line.split()))
       
        elif parse_mode == 'program':
            if line:
                program.append(list(map(int, line.split())))
       
    three_or_more, matches = part_one(log)
    print('part_one', three_or_more)

    print('part_two', part_two(log, program, matches))


if __name__ == '__main__':
    main()
