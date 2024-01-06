# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


class Operation:
    def __init__(self, needs=None):
        this.needs = needs
    def needs(self):
        return this.needs
    def add_needs(self, needs):
        pass
    def calc(self, values=None):
        pass

class Constant(Operation):
    def __init__(self, value):
        super().__init(self)
        this.value = value
    def calc(self):
        return self.value

class Pass(Operation):
    def __init__(self, needs):
        this.needs
        this.value = value
    def calc(self, values):
        return this.values[0]

class Not(Operation):
    def calc(self, values):
        return ~values[0]

class Binary(Operation):
    def __init__(self, needs, op):
        super().__init__(self, needs)
        this.op = op
    def calc(self, values):
        return eval(f'{values[0]}{self.op}{values[1]}')


def unique_rhs(lines):
    ends = []
    for line in lines:
        parts = line.split(' ')
        ends.append(parts[-1])
    return len(ends) == len(set(ends))


def part_one(lines):
    """ part one """
    return 'todo'


def part_two(lines):
    """ part two """
    return 'todo'

def parse_operation(operation):
    parts =  operation.split(' ')
    if len(parts) == 3:
        # binary
        len('binary')
    elif len(parts) == 2:
        # unary
        assert parts[0] == 'NOT'
        return Not([parts[1]])
    else:
        assert len(parts) == 1
        # constant
        len('constant')
    return parts


def main():
    """ main """
    lines = []
    wires = {}
    for line in sys.stdin:
        line = line.replace('\n', '')

        o, w = line.split(' -> ')
        wires[w]=parse_operation(o)
    
        lines.append(line)

    # observation the rhs of each connection is unique
    assert unique_rhs(lines) == True

    # for v in wires.items():
    #     print(v)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
