# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import deque

class Operation:
    def __init__(self, wire, needs=None):
        self.wire = wire
        self.needs = needs
    def calc(self, values):
        pass

class Constant(Operation):
    def calc(self, values):
        return values[0]
    def __repr__(self):
        return f'{self.wire} = {self.needs[0]}'

class Not(Operation):
    def calc(self, values):
        return ~values[0]
    def __repr__(self):
        return f'{self.wire} = ~{self.needs[0]}'

class Binary(Operation):
    def __init__(self, wire, needs, op):
        super().__init__(wire, needs)
        self.op = op
    def calc(self, values):
        return eval(f'{values[0]}{self.op}{values[1]}')
    def __repr__(self):
        return f'{self.wire} = {self.needs[0]} {self.op} {self.needs[1]}'


def unique_rhs(lines):
    ends = []
    for line in lines:
        parts = line.split(' ')
        ends.append(parts[-1])
    return len(ends) == len(set(ends))


def part_one(wires:dict[string,Operation], values=None):
    """ part one """
    q = deque()
    op = wires['a']
    if not values:
        values = {}

    while op:
        needs = [values[n] for n in op.needs if n in values]
        if len(needs) == len(op.needs):
            val = op.calc(needs)
            if op.wire == 'a':
                return val
            values[op.wire] = val
        else:
            q.append(op)
            for n in op.needs:
                if n not in values:
                    if n in wires:
                        q.append(wires[n])
                    else:
                        values[n] = int(n)

        if len(q) > len(wires):
            print('waaaaa')
            print(q)
            break
        op=q.pop()
    return 'todo'


def part_two(wires):
    """ part two """
    a = part_one(wires)
    return part_one(wires,{'b':a})

operations = {
        'AND': '&',
        'OR': '|',
        'LSHIFT': '<<',
        'RSHIFT': '>>',
}

def parse_operation(wire, operation):
    parts =  operation.split(' ')
    if len(parts) == 3:
        op = operations[parts[1]]
        return Binary(wire, [parts[0], parts[2]], op)
    if len(parts) == 2:
        assert parts[0] == 'NOT'
        return Not(wire, [parts[1]])
    if len(parts) == 1:
        return Constant(wire, parts)
    assert False


def main():
    """ main """
    lines = []
    wires = {}
    for line in sys.stdin:
        line = line.replace('\n', '')
        o, w = line.split(' -> ')
        wires[w]=parse_operation(w, o)
        lines.append(line)

    # observation: the rhs of each connection is unique
    assert unique_rhs(lines) is True

    print('part_one', part_one(wires))
    print('part_two', part_two(wires))


main()
