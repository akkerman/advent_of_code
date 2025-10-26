"""Day 20: Firewall Rules."""
import fileinput

Interval = tuple[int, int]

def part_one(intervals: list[Interval]):
    """Solution to part one."""
    to_ip = 0
    for f, t in sorted(intervals, key=lambda x: x[0]):
        if 0 <= f <= to_ip:
            to_ip = max(to_ip, t)
            continue
        elif f == to_ip + 1:
            to_ip = max(to_ip, t)
            continue
        else:
            assert to_ip < f
            return to_ip + 1

    assert False, "Shouldn't reach here"


def part_two(intervals: list[Interval]):
    """Solution to part two."""
    to_ip = 0
    count = 0
    for f, t in sorted(intervals, key=lambda x: x[0]):
        if f > (to_ip + 1):
            count += f - to_ip - 1

        to_ip = max(to_ip, t)

    assert to_ip == 4294967295

    return count


def main():
    """Parse input file, pass to puzzle solvers."""
    intervals: list[Interval] = []
    for line in fileinput.input():
        line = line.strip()
        interval = tuple(map(int, line.split('-')))
        assert len(interval) == 2
        
        intervals.append(interval)

    print('part_one', part_one(intervals))

    ## too high: 202
    print('part_two', part_two(intervals))


if __name__ == '__main__':
    main()
