"""2025 Day 12: Christmas Tree Farm"""
import fileinput
import re

Coord = tuple[int, int]
Present = list[Coord]
BoundingBox = tuple[int, int]
Requirements = list[int]
Region = tuple[BoundingBox, Requirements]

def part_one(presents: dict[int,Present], regions: list[Region]):
    """Solution to part one."""
    present_areas = [len(present) for present in presents.values()]

    count: int = 0
    for region in regions:
        (width, length), num_presents = region
        region_area = width * length

        present_area = sum([n * a for n, a in zip(num_presents, present_areas)])
        if present_area <= region_area:
            count += 1

    return count

re_region = re.compile(r'(.+)x(.+): (.+) (.+) (.+) (.+) (.+) (.+)')

def main():
    """Parse input file, pass to puzzle solvers."""
    present_id = 0
    presents: dict[int, list[Coord]] = {}
    y: int = 0
    regions: list[Region] = []

    for line in fileinput.input():
        if 'x' in line:
            m = re_region.match(line)
            assert m is not None
            ints = [int(i) for i in m.groups()]
            region: Region = ((ints[0], ints[1]), ints[2:])
            regions.append(region)
        else:

            if ':' in line:
                present_id = int(line.split(':')[0])
                presents[present_id] = []
                y = 0
                continue

            for x, c in enumerate(line.strip()):
                if c == '#':
                    presents[present_id].append((x, y))
            y += 1


          

            
    for present in presents:
        print(present, ':',  presents[present])

    for region in regions:
        print(region)   


    print('part_one', part_one(presents, regions))

if __name__ == '__main__':
    main()
