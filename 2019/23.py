"""Day 23: Category Six."""
import fileinput
from abc import abstractmethod
from collections import deque
from intcode import IO
import threading
from collections import defaultdict


def computer(program: list[int] | defaultdict[int,int], io:IO):
    """Computer."""
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    ADJUST_RELATIVE_BASE = 9
    HALT = 99

    MODE_POSITION = 0
    MODE_IMMEDIATE = 1
    MODE_RELATIVE = 2

    modes = [0,0,0,0]

    relative_base = 0
    
    if type(program) is list:
        program = defaultdict(int, enumerate(program))

    def write(instruction_pointer:int, parameter:int, value:int):
        # parameters that an instruction writes to wil never be in immediate mode
        if modes[parameter] == MODE_POSITION:
            program[program[instruction_pointer+parameter]] = value
        elif modes[parameter] == MODE_RELATIVE:
            program[program[instruction_pointer+parameter] + relative_base] = value
        else:
            raise ValueError('Invalid mode for writing {}'.format(modes[parameter]))


    def read(instruction_pointer:int, parameter:int):
        if modes[parameter] == MODE_POSITION:
            return program[program[instruction_pointer + parameter]]
        elif modes[parameter] == MODE_IMMEDIATE:
            return program[instruction_pointer + parameter]
        elif modes[parameter] == MODE_RELATIVE:
            return program[program[instruction_pointer + parameter] + relative_base]
        else:
            raise ValueError('Invalid mode for reading {}'.format(modes[parameter]))

    i = 0
    def run():

        nonlocal modes
        nonlocal relative_base
        nonlocal i

        while i < len(program):
            p3, p2, p1, o1, o2 = list(str(program[i]).zfill(5))
            modes = [0, int(p1), int(p2), int(p3)]
            op = int(o1 + o2)
            if op == HALT:
                break
            elif op == ADD:
                write(i, 3, read(i, 1) + read(i, 2))
                i+=4
            elif op == MULTIPLY:
                write(i, 3, read(i, 1) * read(i, 2))
                i+=4
            elif op == INPUT:
                value = io.input()
                write(i, 1, value)
                i+=2
                if value == -1:
                    break
            elif op == OUTPUT:
                io.output(read(i, 1))
                i+=2
            elif op == JUMP_IF_TRUE:
                if read(i, 1) != 0:
                    i = read(i, 2)
                else:
                    i+=3
            elif op == JUMP_IF_FALSE:
                if read(i, 1) == 0:
                    i = read(i, 2)
                else:
                    i+=3
            elif op == LESS_THAN:
                write(i, 3, 1 if read(i, 1) < read(i, 2) else 0)
                i+=4
            elif op == EQUALS:
                write(i, 3, 1 if read(i, 1) == read(i, 2) else 0)
                i+=4
            elif op == ADJUST_RELATIVE_BASE:
                relative_base += read(i, 1)
                i+=2
            else:
                raise ValueError('Unknown opcode {}'.format(op))

    return run


class Network:
    @abstractmethod
    def send(self, address:int, x:int, y:int):
        pass


class NIC(IO):
    def __init__(self, program: list[int], address:int, network: Network):
        self.address = address
        self.run = computer(program, self)
        self.network = network
        self.data = deque[int]([address])
        self.packets = list[int]()

    def input(self) -> int:
        if self.data:
            return self.data.popleft()
        return -1

    def output(self, value: int):
        # print('NIC', self.address, 'received', value)
        self.packets.append(value)
        if len(self.packets) == 3:
            address, x, y = self.packets
            self.packets.clear()
            self.network.send(address, x, y)

class Lan(Network):
    def __init__(self, program: list[int], size:int):
        self.nics = [NIC(program, i, self) for i in range(size)]
        self.threads = list[threading.Thread]()
        self.solution_part1 = None
        self.solution_part2 = None
        self.sent_y = None
        self.running = True

    def send(self, address:int, x:int, y:int):
        if address == 255:
            if self.solution_part1 is None:
                self.solution_part1 = y
                print('Part one:', self.solution_part1)

            if not any(nic.data for nic in self.nics):
                if y == self.sent_y: # type: ignore
                    self.solution_part2 = y
                    print('Part two:', self.solution_part2)
                    self.running = False
                self.nics[0].data.append(x)
                self.nics[0].data.append(y)
                self.sent_y = y
        else:
            self.nics[address].data.append(x)
            self.nics[address].data.append(y)

    def run(self):
        while self.running:
            for nic in self.nics:
                nic.run()


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    Lan(program, 50).run()


if __name__ == '__main__':
    main()
    print('done')
