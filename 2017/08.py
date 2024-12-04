# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
from collections import defaultdict
import sys
import re
import operator

from typing import List, Tuple, Callable

InputLine = Tuple[str, str, str, str, str, str]
Instruction = Tuple[str, str, int, str, Callable[[int,int],bool], int]

comparison_operators = {
    '<': operator.lt,
    '>': operator.gt,
    '<=': operator.le,
    '>=': operator.ge,
    '==': operator.eq,
    '!=': operator.ne,
}

def part_one(registers: dict[str, int], instructions:List[Instruction]) -> int:
    """ part one """
    for reg, op, val, evalReg, evalOp, evalVal in instructions:
        if evalOp(registers[evalReg], evalVal):
            if op == 'inc':
                registers[reg] += val
            else:
                registers[reg] -= val

    return max(registers.values())


def part_two(registers: dict[str, int], instructions:List[Instruction]) -> int:
    """ part two """
    _ = registers
    _ = instructions
    return -1

pattern = r'([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (>|<|>=|<=|==|!=) (-?[0-9]+)'

def to_instruction(line: InputLine) -> Instruction:
    """ instructions """
    a, b, c, d, e, f = line
    return (a, b, int(c), d, comparison_operators[e], int(f)) 

def main():
    """ main """
    instructions: List[Instruction] = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        
        m = re.match(pattern, line)
        if m is None:
            continue

        instructions.append(to_instruction(m.groups())) # type: ignore


    d: dict[str, int] = defaultdict(int)
    print('part_one', part_one(d, instructions))

    d: dict[str, int] = defaultdict(int)
    print('part_two', part_two(d, instructions))


main()
