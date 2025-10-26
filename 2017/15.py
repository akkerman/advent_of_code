"""Day 15: Dueling Generators."""
import fileinput
import re

factor_a = 16807
factor_b = 48271

def next_value(previous:int, factor:int):
    """Generate the next value in the sequence."""
    modulus = 2147483647
    return (previous * factor) % modulus

def next_a_value(previous:int):
    """Generate the next value for generator A."""
    curr = previous
    while True:
        curr = next_value(curr, factor_a)
        if curr % 4 == 0:
            return curr

def next_b_value(previous:int):
    """Generate the next value for generator B."""
    curr = previous
    while True:
        curr = next_value(curr, factor_b)
        if curr % 8 == 0:
            return curr

def part_one(gen_a:int, gen_b:int):
    """Solution to part one."""
    curr_a: int = gen_a
    curr_b: int = gen_b

    count = 0

    for _ in range(40_000_000):
        curr_a = next_value(curr_a, factor_a) 
        curr_b = next_value(curr_b, factor_b)

        # witwise and with full 16 bits, results in last 16 bits
        if (curr_a & 0xFFFF) == (curr_b & 0xFFFF):
            count += 1

    return count

def part_two(gen_a:int, gen_b:int):
    """Solution to part two."""
    curr_a: int = gen_a
    curr_b: int = gen_b

    count = 0

    for _ in range(5_000_000):
        curr_a = next_a_value(curr_a)
        curr_b = next_b_value(curr_b)

        # witwise and with full 16 bits, results in last 16 bits
        if (curr_a & 0xFFFF) == (curr_b & 0xFFFF):
            count += 1

    return count

def main():
    """Parse input file, pass to puzzle solvers."""
    generators: list[int] = []
    for line in fileinput.input():
        line = line.strip()

        # match the numer at the end of the line
        matched =  re.search(r'(\d+)$', line)
        assert matched is not None
        start_value = int(matched.group(1))
        
        generators.append(start_value)

    print('part_one', part_one(*generators))

    # to high: 2484
    print('part_two', part_two(*generators))


if __name__ == '__main__':
    main()
