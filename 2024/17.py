"""Day 17: Chronospatial Computer."""
import sys
import re


A = 0
B = 1
C = 2

def part_one(registers:list[int], program:list[int])->list[int]:
    """Solution to part one."""
    out: list[int] = []
    i = 0
    while True:
        if i >= len(program):
            break

        def read_value(operand:int) -> int:
            if 0 <= operand <= 3:
                return operand
            if operand == 4:
                return registers[A]
            if operand == 5:
                return registers[B]
            if operand == 6:
                return registers[C]
            raise ValueError(f'invalid operand {operand}')

        opcode, operand = program[i], program[i+1]

        if opcode == 0: # adv: division to 
            registers[A] = registers[A] // (2 ** read_value(operand))
            i += 2
        elif opcode == 1: # bxl: bitwise XOR
            registers[B] = registers[B] ^ operand
            i += 2
        elif opcode == 2: # bst: mod 8
            registers[B] = read_value(operand) % 8
            i += 2
        elif opcode == 3: # jnz: noop
            if registers[A] == 0:
                i += 2
            else:
                i = operand
        elif opcode == 4: # bxc: bitwise XOR
            registers[B] = registers[B] ^ registers[C]
            i += 2
            pass
        elif opcode == 5: # out: mod 8
            out.append(read_value(operand) % 8)
            i += 2
            pass
        elif opcode == 6: # bdv: division to B
            registers[B] = registers[A] // (2 ** read_value(operand))
            i += 2
            pass
        elif opcode == 7: # cdv: division to C
            registers[C] = registers[A] // (2 ** read_value(operand))
            i += 2
            pass

    return out


def part_two(registers:list[int], program:list[int])->int:
    """Solution to part two."""
    target = part_one(registers.copy(), program)

    registers[A] = 117440
    print(part_one(registers.copy(), program))

    while True:
        registers[A] += 1
        out = part_one(registers.copy(), program)
        if out == target:
            return registers[A]


def main():
    """Parse input file, pass to puzzle solvers."""
    registers: list[int] = []
    program: list[int] = []
    parsing = 'registers'
    for line in sys.stdin:
        line = line.strip()
        if not line:
            parsing = 'program'
            continue
        if parsing == 'registers':
            registers.append(int(re.findall(r'(\d+)', line)[0]))
        else:
            program = list(map(int, re.findall(r'(\d+)', line)))

    output = part_one(registers.copy(), program)
    print('part_one', ",".join(map(str,output)))

    reg = registers.copy()
    reg[A] = 117440
    output = part_one(reg, program)
    print('test', ",".join(map(str,output)))

    # print('part_two', part_two(registers.copy(), program))


main()
