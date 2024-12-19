"""Day 19: Linen Layout."""
import sys
from functools import lru_cache

def solve(patterns:list[str], designs:list[str]):
    @lru_cache
    def create(design:str)->int:
        if design == '':
            return 1
        count = 0
        for pattern in patterns:
            if design.startswith(pattern):
                created = create(design[len(pattern):])
                if created > 0:
                    count += created
        return count

    matches = [create(design) for design in designs]

    print('part_one',  sum([m > 0 for m in matches]))
    print('part_two',  sum(matches))


def main():
    """Parse input file, pass to puzzle solvers."""
    patterns:list[str] = []
    designs:list[str] = []
    parsing = 'patterns'
    for line in sys.stdin:
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

