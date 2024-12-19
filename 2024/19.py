"""Day 19: Linen Layout."""
import fileinput
from functools import lru_cache
from utils import perf_timer

@perf_timer
def solve(patterns:list[str], designs:list[str]):
    """Check if and how many designs can be created from patterns."""
    @lru_cache(maxsize=len(patterns))
    def count_possible_designs(design:str)->int:
        """Count how many designs can be created from patterns."""
        if design == '':
            return 1
        count = 0
        for pattern in patterns:
            if not design.startswith(pattern): continue
            created = count_possible_designs(design[len(pattern):])
            if created == 0: continue
            count += created
        return count

    matches = [count_possible_designs(design) for design in designs]

    print('part_one',  sum([m > 0 for m in matches]))
    print('part_two',  sum(matches))


def main():
    """Parse input file, pass to puzzle solvers."""
    patterns:list[str] = []
    designs:list[str] = []
    parsing = 'patterns'
    for line in fileinput.input():
        line = line.strip()
        if not line:
            parsing = 'designs'
            continue

        if parsing == 'patterns':
            patterns = line.split(', ')
        else:
            designs.append(line)

    solve(patterns, designs)

main()

