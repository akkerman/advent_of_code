"""Day 25: Code Chronicle."""
import fileinput


def fit(lock: list[int], key: list[int]) -> bool:
    """Check if key fits lock."""
    for l, k in zip(lock, key):
        if l + k > 5:
            return False
    return True

def part_one(schematics: list[list[str]]):
    """Determine number of unique lock/key combinations."""
    locks: list[list[int]] = []
    keys: list[list[int]] = []

    for schematic in schematics:
        colums: list[int] = [-1]*len(schematic[0])
        for line in schematic:
            for i, char in enumerate(line):
                if char == '#':
                    colums[i] += 1
        if schematic[0] == '#' * len(schematic[0]):
            locks.append(colums)
        else:
            keys.append(colums)

    return sum(fit(lock, key) for lock in locks for key in keys)

def main():
    """Parse input file, pass to puzzle solvers."""
    schematics: list[list[str]] = []
    schematic: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        if  not line:
            schematics.append(schematic)
            schematic = list()
        else:
            schematic.append(line)

    schematics.append(schematic)

    print('part_one', part_one(schematics))


if __name__ == '__main__':
    main()
