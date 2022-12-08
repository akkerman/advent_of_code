import sys
from typing import DefaultDict

def parse(lines):
    path = []
    dirs = DefaultDict(int)
    for line in lines:
        if line.startswith('$ cd'):
            [_, _, dirname] = line.split(' ')
            if dirname == '/':
                path = []
            elif dirname == '..':
                path.pop()
            else:
                path.append(dirname)
            continue

        if line.startswith('$ ls'):
            continue

        [size, name] = line.split(' ')
        if size == 'dir':
            dirs['/'.join(path + [name])] += 0
        else:
            tmppath = path[:]
            while True: # add file size to all parent dirs
                dirs['/'.join(tmppath)] += int(size)
                if not tmppath:
                    break
                tmppath.pop()

    return dirs

def part_one(dirs):
    dirs[''] = 0
    return sum([size for size in dirs.values() if size < 100000])

def part_two(dirs):
    unused = 70000000 - dirs['']
    freeup = 30000000 - unused
    return min([size for size in dirs.values() if size >= freeup])


def main():
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    print('part_one', part_one(parse(lines)))

    print('part_two', part_two(parse(lines)))


main()
