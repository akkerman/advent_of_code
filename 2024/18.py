"""Day 18: RAM Run."""
import sys
import heapq

from utils import perf_timer


def print_memory(bytes: list[tuple[int,int]], exit:tuple[int,int]):
    """Print memory."""
    for y in range(exit[1]+1):
        for x in range(exit[0]+1):
            if (x,y) in bytes:
                print('#', end='')
            else:
                print('.', end='')
        print()


def part_one(bytes:list[tuple[int,int]], count:int=1024, exit:tuple[int,int]=(70,70)):
    """Shortest path after letting the indicated number of bytes fall."""
    fallen = set(bytes[:count])
    xr, xc = exit

    def in_grid(r:int, c:int) -> bool:
        return 0 <= r <= xr and 0 <= c <= xc

    def neigbor(r:int, c:int) -> set[tuple[int,int]]:
        candidates = { (r+1, c), (r-1, c), (r, c+1), (r, c-1) }
        return { (r,c) for r,c in candidates if in_grid(r,c) and (r,c) not in fallen }

    q = [(0,0,0)]
    visited: set[tuple[int,int]] = set()
    while q:
        steps, r, c = heapq.heappop(q)
        for nr, nc in neigbor(r, c):
            if (nr, nc) == exit:
                return steps + 1
            if (nr, nc) in visited:
                continue
            visited.add((nr, nc))
            heapq.heappush(q, (steps+1, nr, nc))
    
    return -1


@perf_timer
def part_two(memory:list[tuple[int,int]], start_count: int=1024, exit:tuple[int,int]=(70,70)):
    """First fallen byte that blocks the exit."""
    for count in range(start_count, len(memory)):
        if part_one(memory, count, exit) == -1:
            return memory[count-1]

def main():
    """Parse input file, pass to puzzle solvers."""
    bytes:list[tuple[int,int]] = []
    for line in sys.stdin:
        line = tuple(map(int, line.strip().split(',')))
        bytes.append(line) # type: ignore


    # example
    # print('part_one', part_one(bytes, 12, (6,6)))

    # input
    print('part_one', part_one(bytes))

    # example
    # print('part_two', part_two(bytes, start_time=12, exit=(6,6)))

    # input
    print('part_two', part_two(bytes))


main()
