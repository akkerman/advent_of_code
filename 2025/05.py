"""2025 Day 5: Cafeteria"""
import fileinput

def part_one(ranges: list[tuple[int,int]], ingredients:list[int]):
    """Solution to part one."""
    fresh = 0
    for ingredient in ingredients:
        for f,t in ranges:
            if f <= ingredient <= t:
                break
        else: 
            fresh += 1
    return len(ingredients) - fresh


def part_two(ranges: list[tuple[int,int]]):
    """Solution to part two."""
    sorted_uniq = sorted(set(ranges), key=lambda x: x[0])

    fresh = 0
    prev: tuple[int,int] = sorted_uniq[0]

    for f,t in sorted_uniq[1:]:
        if f <= prev[1]:
            prev = (prev[0], max(prev[1], t))
        else:
            fresh += prev[1] - prev[0] + 1
            prev = (f,t)

    return fresh + (prev[1] - prev[0] + 1)



def main():
    """Parse input file, pass to puzzle solvers."""
    ranges = list[tuple[int, int]]()
    ingredients = list[int]()

    parse_state = 'ranges'

    for line in fileinput.input():
        line = line.strip()

        if not line:
            parse_state = 'ingredients'
            continue

        if parse_state == 'ranges':
            f,t = list(map(int, line.split('-')))
            ranges.append((f,t))
        elif parse_state == 'ingredients':
            ingredients.append(int(line))
        

    print('part_one', part_one(ranges, ingredients))

    print('part_two', part_two(ranges))


if __name__ == '__main__':
    main()
