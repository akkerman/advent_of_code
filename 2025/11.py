"""2025 Day 11: Reactor"""
import fileinput
from collections import deque
from functools import lru_cache

def part_one(devices: dict[str, list[str]]):
    """Solution to part one."""
    queue = deque[list[str]]()
    queue.append(['you'])

    paths = set[str]()
    while queue:
        path = queue.popleft()
        node = path[-1]

        for dest in devices[node]:
            if dest == 'out':
                paths.add(','.join(path + [dest]))
                continue
            if dest in path:
                continue
            new_path = list(path)
            new_path.append(dest)
            queue.append(new_path)

    return len(paths)

def part_two(devices: dict[str, list[str]]):
    """Solution to part two."""
    @lru_cache(maxsize=None)
    def dfs(src:str, dst:str) -> int:
        if src == dst:
            return 1
        
        return sum(dfs(nxt, dst) 
                   for nxt in devices.get(src, []))
    
    return (
        dfs('svr', 'fft') * dfs('fft', 'dac') * dfs('dac', 'out') +
        dfs('svr', 'dac') * dfs('dac', 'fft') * dfs('fft', 'out') 
    )



def main():
    """Parse input file, pass to puzzle solvers."""
    devices = dict[str, list[str]]()
    for line in fileinput.input():
        line = line.strip()
        df, dt = line.split(': ')
        dt = dt.split(' ')
        devices[df] = dt
        

    print('part_one', part_one(devices))

    print('part_two', part_two(devices))


if __name__ == '__main__':
    main()
