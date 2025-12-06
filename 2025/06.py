"""2025 Day 6: Trash Compactor"""
import fileinput
import math

def calculate_problem(numbers: list[int], operator:str) -> int:
    """Calculate the problem based on numbers and operator."""
    if operator == '+':
        return sum(numbers)
    elif operator == '*':
        return math.prod(numbers)
    else:
        assert False, f'Unknown operator {operator}'

def part_one(problems: list[list[int]], operators: list[str]):
    """Solution to part one."""
    return sum(calculate_problem(p, o) for p, o in zip(problems, operators))


def part_two(lines: list[str], operators: list[str]) -> int:
    """Solution to part two."""
    chars = [list(line) for line in lines]
    # rotate 90 degrees anti-lockwise
    rotated = list(zip(*chars))[::-1]

    print(rotated)
    
    nums = list[list[int]]()

    for row in rotated:
        num_str = ''.join(row).strip()
        if not num_str:
            nums.append([])
            continue
        nums[-1].append(int(num_str))

    return part_one(nums, operators[::-1])



def main():
    """Parse input file, pass to puzzle solvers."""
    problems: list[list[int]] = []
    operators: list[str] = []
    lines: list[str] = []

    for line in fileinput.input():
        lines.append(line)
        parts = line.strip().split()
        if parts[0].isdigit():
            for i, num in enumerate(parts):
                if len(problems) <= i:
                    problems.append([])
                problems[i].append(int(num))
        else:
            operators = parts



    print('part_one', part_one(problems, operators))

    print('part_two', part_two(lines[:-1], operators))


if __name__ == '__main__':
    main()
