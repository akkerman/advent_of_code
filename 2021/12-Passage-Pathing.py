import sys

adjacent = {}

for line in sys.stdin:
    a,b = line.replace('\n', '').split('-')
    
    if not a in adjacent:
        adjacent[a] = []
    if not b in adjacent:
        adjacent[b] = []

    adjacent[a].append(b)
    adjacent[b].append(a)

def find_paths(start, end, paths=[], currentPath=[], visited = set()):
    if start == end:
        paths.append(currentPath + [end])
        return paths

    if start in visited:
        return paths

    my_visited = visited | {start} if start.islower() else visited
    my_path = currentPath + [start]

    for next in adjacent[start]:
        find_paths(next, end, paths, my_path, my_visited)

    return paths

def find_paths2(start, end, paths=[], currentPath=[], visited = set(), double=False):
    if start == end:
        paths.append(currentPath + [end])
        return paths

    if start in visited and (start == 'start' or double):
        return paths

    my_visited = visited
    if start.islower():
        if start in visited:
            double = True
        else:
            my_visited = visited | {start}

    my_path = currentPath + [start]

    for next in adjacent[start]:
        find_paths2(next, end, paths, my_path, my_visited, double)

    return paths

def part_one():
    paths = find_paths('start', 'end')

    print(len(paths))

def part_two():
    paths = find_paths2('start', 'end')

    print(len(paths))

print(part_one())
print(part_two())
