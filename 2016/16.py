"""Day 16: Dragon Checksum."""
import fileinput

flip = {'0': '1', '1': '0'}
def step(a:str) -> str:
    """Generate the next data string."""
    b = a[::-1]
    b = "".join(flip[x] for x in b)
    return a + '0' + b

def checksum_1(data:str) -> str:
    """Generate one checksum iteration."""
    c = ''
    for x,y in zip(data[::2], data[1::2]):
        if x == y:
            c += '1'
        else:
            c += '0'
    return c

def checksum(data:str) -> str:
    """Generate checksum of odd length."""
    while True:
        data = checksum_1(data)
        if len(data) % 2 == 1:
            return data

def fill_disk(data:str, size:int) -> str:
    """Fill disk to size."""
    while len(data) < size:
        data = step(data)
    return data[:size]


def part_one(data:str, disk_size:int) -> str:
    """Generate checksum of filled disk of given size."""
    filled_disk = fill_disk(data, disk_size)
    return checksum(filled_disk)


def part_two(line:str):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    line: str = ''
    for line in fileinput.input():
        line = line.strip()

    print('part_one', part_one(line, 272))

    print('part_two', part_two(line))


if __name__ == '__main__':
    main()

def test_step_1():
    assert step('1') == '100'
def test_step_0():
    assert step('0') == '001'
def test_step_11111():
    assert step('11111') == '11111000000'
def test_step_111100001010():
    assert step('111100001010') == '1111000010100101011110000'

def test_checksum_110010110100():
    assert checksum('110010110100') == '100'

def test_fill_disk_20():
    assert fill_disk('10000', 20) == '10000011110010000111'

def test_part_one_example():
    assert part_one('10000', 20) == '01100'
