"""Day 7: Amplification Circuit."""
from collections import deque
import sys
from itertools import permutations

def computer(program: list[int], phase_setting: int):
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    HALT = 99

    modes = [0,0,0,0]
    i = 0 # instruction pointer

    phase_setting_applied = False

    def run(input:int)->int|None:
        nonlocal modes
        nonlocal i
        nonlocal phase_setting_applied

        def write(instruction_pointer:int, parameter:int, value:int):
            # parameters that an instruction writes to wil never be in immediate mode
            program[program[instruction_pointer+parameter]] = value

        def read(instruction_pointer:int, parameter:int):
            if modes[parameter]:
                # immediate mode
                return program[instruction_pointer + parameter]
            else:
                # position mode
                return program[program[instruction_pointer + parameter]]

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
                if not phase_setting_applied:
                    write(i, 1, phase_setting)
                    phase_setting_applied = True
                else:
                    write(i, 1, input)
                i+=2
            elif op == OUTPUT:
                output = read(i, 1)
                i+=2
                return output
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
            else:
                ValueError('Unknown opcode {}'.format(op))

        return None

    return run

def amplifiers(acs: list[int], phase_setting: list[int]) -> int:
    """Run the 5 amplifiers with the given phase setting."""
    value = 0
    ps = deque(phase_setting)
    for _ in range(5):
        value = computer(acs.copy(), ps.popleft())(value)
        assert value is not None
    return value



def feedback(acs: list[int], phase_setting: list[int]) -> int:
    """Run the 5 amplifiers with the given phase setting in feedback mode."""
    value = 0
    output = 0
    ps = deque(phase_setting)
    amp = [computer(acs.copy(), ps.popleft()) for _ in range(5)]
    amp_index = 0
    while output is not None:
        value = output
        output = amp[amp_index](value)
        amp_index = (amp_index + 1) % 5
    return value



def part_one(acs: list[int]) -> tuple[int, list[int]]:
    """Find maximum signal that can be sent to the thrusters."""
    signal = 0
    phase_setting = []

    for perm in permutations(range(5)):
        setting = list(perm)
        output = amplifiers(acs.copy(), setting.copy())
        if output > signal:
            signal = output
            phase_setting = setting

    return signal, phase_setting


def part_two(acs: list[int]):
    """Solution to part two."""
    signal = 0
    phase_setting = []

    for perm in permutations(range(5, 10)):
        setting = list(perm)
        output = feedback(acs.copy(), setting.copy())
        if output > signal:
            signal = output
            phase_setting = setting

    return signal, phase_setting


def main():
    """Parse input file, pass to puzzle solvers."""
    acs: list[int] = [] # amplifier controller software
    for line in sys.stdin:
        line = line.strip()
        line = list(map(int, line.split(',')))
        acs.extend(line)

    print('part_one', part_one(acs.copy()))

    print('part_two', part_two(acs))

if __name__ == '__main__':
    main()

# part one tests
def test_amplifiers1():
    assert amplifiers([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0], [4,3,2,1,0]) == 43210
def test_amplifiers2():
    assert amplifiers([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0], [0,1,2,3,4]) == 54321
def test_amplifiers3():
    assert amplifiers([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0], [1,0,4,3,2])
def test_part_one_1():
    assert part_one([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) == (43210, [4,3,2,1,0 ])
def test_part_one_2():
    assert part_one([3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) == (54321, [0,1,2,3,4 ])
def test_part_one_3():
    assert part_one([3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) == (65210, [1,0,4,3,2 ])

# part two tests
def test_feedback1():
    assert feedback([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5], [9,8,7,6,5]) == 139629729
def test_feedback2():
    assert feedback([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10], [9,7,8,5,6]) == 18216
def test_part_two1():
    assert part_two([3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]) == (139629729, [9,8,7,6,5])
def test_part_two2():
    assert part_two([3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]) == (18216, [9,7,8,5,6])
