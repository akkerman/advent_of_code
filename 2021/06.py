"""Day 6: Lanternfish."""
import sys
import string


def solve(input: list[int], days: int) -> int:
    """Determine number of lanterfish after given days."""
    lanternfish = [0] * 9
    for c in input:
        lanternfish[c] += 1

    for _ in range(days):
        breeders, *rest = lanternfish
        lanternfish = rest + [breeders]
        lanternfish[6] += breeders

    return sum(lanternfish)

def main():
    """Parse input file, pass to puzzle solvers."""
    input: list[int] = []
    for line in sys.stdin:
        line = line.strip()
        input = list(map(int, line.split(',')))
        break
        

    print('part_one', solve(input, 80))

    print('part_two', solve(input, 256))


main()
