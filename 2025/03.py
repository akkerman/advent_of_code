"""2025 Day 3: Lobby"""
import fileinput

Battery = list[int]

def max_joltage_part1(battery: Battery) -> int:
    """Return the maximum joltage of the battery."""
    tiental = max(battery[:-1])
    idx = battery.index(tiental)
    eenheid = max(battery[idx + 1 :])
    return 10 * tiental  + eenheid

def part_one(batteries: list[Battery]):
    """Solution to part one."""
    return sum(max_joltage_part1(battery) for battery in batteries)

def max_joltage_part2(battery: Battery) -> int:
    """Return the maximum joltage of the battery."""
    mj = 0

    for i in range(11, 0, -1):
        mx = max(battery[:-i])
        mj = mj * 10 + mx
        battery = battery[battery.index(mx) + 1 :]
    return 10 * mj + max(battery)


def part_two(batteries: list[Battery]):
    """Solution to part two."""
    return sum(max_joltage_part2(battery) for battery in batteries)


def main():
    """Parse input file, pass to puzzle solvers."""
    batteries: list[Battery] = []
    for line in fileinput.input():
        batery = [int(x) for x in line.strip()]
        batteries.append(batery)

    print('part_one', part_one(batteries))

    print('part_two', part_two(batteries))


if __name__ == '__main__':
    main()
