"""Day 17: Chronospatial Computer."""
import sys
import re


A = 0
B = 1
C = 2

def part_one(registers:list[int], program:list[int])->list[int]:
    """Solve part one with an int computer"""
    out: list[int] = []
    i = 0
    while True:
        if i >= len(program):
            break

        def combo(operand:int) -> int:
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

        if opcode == 0: # adv: A division
            registers[A] = registers[A] >> combo(operand)
            i += 2
        elif opcode == 1: # bxl: B xor literal
            registers[B] = registers[B] ^ operand
            i += 2
        elif opcode == 2: # bst: mod 8
            registers[B] = combo(operand) % 8
            i += 2
        elif opcode == 3: # jnz: jump not zero
            if registers[A] == 0:
                i += 2
            else:
                i = operand
        elif opcode == 4: # bxc: B xor C
            registers[B] = registers[B] ^ registers[C]
            i += 2
            pass
        elif opcode == 5: # out: mod 8
            out.append(combo(operand) % 8)
            i += 2
            pass
        elif opcode == 6: # bdv: B division
            registers[B] = registers[A] >> combo(operand)
            i += 2
            pass
        elif opcode == 7: # cdv: C division
            registers[C] = registers[A] >> combo(operand)
            i += 2
            pass

    return out


def part_one_alt(registers:list[int], _:list[int])->list[int]:
    """Solve part one with direct computation."""
    a = registers[A]
    b = 0
    c = 0

    out: list[int] = []
    while a:
       b = a % 8
       b = b ^ 1
       c = a >> b
       b = b ^ 5
       b = b ^ c
       a = a >> 3
       out.append(b % 8)

    return out





def part_two(program:list[int], reg_a: int)->int|None:
    if program == []:
        return reg_a

    for offset in range(8):
       a = (reg_a << 3) + offset
       b = a % 8
       b = b ^ 1
       c = a >> b
       b = b ^ 5
       b = b ^ c
       if b % 8 == program[-1]:
           # a lowest value found, try to solve previous loops
           prev = part_two(program[:-1], a)
           if prev: return prev

    return None


def commas(output: list[int]) -> str:
    """Convert list of ints to comma separated string."""
    return ",".join(map(str, output))

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

    
    print('part_one', commas(part_one(registers.copy(), program)))

    print('part_alt', commas(part_one_alt(registers.copy(), program)))

    p2 = part_two(program.copy(), 0)
    print('part_two', p2)

    registers[A] = p2
    print('solved ', commas(part_one_alt(registers.copy(), program)))


main()
