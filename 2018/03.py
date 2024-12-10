import sys
import re

PATTERN = re.compile(r'#(\d+) @ (\d+),(\d+): (\d+)x(\d+)')

Claim = tuple[int, int, int, int, int]
Area = set[tuple[int, int]]



def claim_to_coords(claim: Claim) -> Area:
    """Convert a claim to a set of coordinates."""
    return {
        (x, y)
        for x in range(claim[1], claim[1] + claim[3])
        for y in range(claim[2], claim[2] + claim[4])
    }

def solve(claims: list[Claim]):
    """Determine total overlapping area of claims."""
    total_area: Area = set()
    claim_areas = [claim_to_coords(claim) for claim in claims]

    found = -1
    for i, c1 in enumerate(claim_areas):
        overlap = False
        for c2 in claim_areas:
            if c1 is c2:
                continue
            if c1 & c2:
                overlap = True
                total_area |= c1 & c2

        if not overlap:
            found = i + 1

    print(f"Part 1: {len(total_area)}")
    print(f"Part 2: {found}")


def main():
    """Parse input file, pass to puzzle solvers."""
    claims: list[Claim] = []
    for line in sys.stdin:
        line = tuple(map(int, PATTERN.match(line.strip()).groups())) # type: ignore
        claims.append(line) # type: ignore

    solve(claims)




main()
