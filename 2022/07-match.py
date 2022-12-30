# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from collections import defaultdict


def parse(lines):
    path = []
    dirs = defaultdict(int)
    for line in lines:
        match line:
            case ["$", "cd", "/"]:
                path = []
            case ["$", "cd", ".."]:
                path.pop()
            case ["$", "cd", name]:
                path.append(name)
            case ["$", "ls"]:
                continue
            case ["dir", name]:
                dirs['/'.join(path + [name])] += 0
            case [size, name]:
                tmppath = path[:]
                while True:  # add file size to all parent dirs
                    dirs['/'.join(tmppath)] += int(size)
                    if not tmppath:
                        break
                    tmppath.pop()
    return dirs


def part_one(dirs):
    """ part one """
    dirs[''] = 0
    return sum([size for size in dirs.values() if size < 100000])


def part_two(dirs):
    """ part two """
    unused = 70000000 - dirs['']
    freeup = 30000000 - unused
    return min([size for size in dirs.values() if size >= freeup])


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '').split(' ')
        lines.append(line)

    print('part_one', part_one(parse(lines)))

    print('part_two', part_two(parse(lines)))


main()
