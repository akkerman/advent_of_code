"""Day 13: Packet Scanners."""
import fileinput

def position_simulation(picosecond:int, depth:int):
    dir = 1
    pos = 0
    for _ in range(picosecond):
        pos += dir
        if pos == 0:
            dir = 1
        if pos == depth - 1:
            dir = -1
    return pos

def position(picosecond:int, depth:int):
    m =((depth-1)*2)
    r = picosecond % m
    if r < depth:
        return r
    return m - r

def part_one(firewall: dict[int,int], delay:int=0):
    """Solution to part one."""
    cost: int = 0
    hit = False
    for level in range(1+max(firewall)):
        if level not in firewall:
            continue
        depth = firewall[level]
        pos = position(level+delay, depth)
        if pos == 0:
            hit = True
            cost += level * depth

    return cost, hit


def part_two(firewall: dict[int,int]):
    """Solution to part two."""
    delay: int = 0
    while True:
        delay += 1
        _, hit = part_one(firewall, delay)
        if not hit:
            break


    return delay


def main():
    """Parse input file, pass to puzzle solvers."""
    firewall: dict[int,int] = dict()
    for line in fileinput.input():
        line = line.strip()
        scanner_depth, scanner_range  = list(map(int, line.split(': ')))

        firewall[scanner_depth] = scanner_range
        
    print('part_one', part_one(firewall))
    print('part_two', part_two(firewall))


if __name__ == '__main__':
    main()
