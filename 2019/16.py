"""Day 16: Flawed Frequency Transmission."""
import fileinput
from functools import lru_cache

base_pattern = [0, 1, 0, -1]

@lru_cache
def pattern(pos: int) -> list[int]:
    return [x for x in base_pattern for _ in range(pos)]

def phase(fft: list[int]) -> list[int]:
    new_fft: list[int] = [0] * len(fft)
    for i in range(len(fft)):
        pix = 1
        pat = pattern(i+1)
        for e in fft:
            new_fft[i] += e * pat[pix]
            pix += 1
            pix = pix % len(pat)

    return [abs(x) % 10 for x in new_fft]

def phase2(fft: list[int]) -> list[int]:
    new_fft = list[int]()

    total = 0
    for x in reversed(fft):
        total += x
        new_fft.append(total % 10)

    return list(reversed(new_fft))


def part_one(inp: str) -> str:
    """Solution to part one."""
    fft = [int(x) for x in inp]
    for _ in range(100):
        fft = phase(fft)
    return ''.join(map(str, fft[:8]))


def part_two(inp: str) -> str:
    """Solution to part two."""
    offset = int(inp[:7])
    fft = [int(x) for x in inp] * 10000
    fft = fft[offset:]
    for _ in range(100):
        fft = phase2(fft)
    return ''.join(map(str, fft[:8]))


def main():
    """Parse input file, pass to puzzle solvers."""
    fft = ""
    for line in fileinput.input():
        fft = line.strip()

    print('part_one', part_one(fft))

    print('part_two', part_two(fft))


if __name__ == '__main__':
    main()
    print('done')


class Test_pattern:

    def test_pattern_1(self):    
        assert pattern(1) == [0, 1, 0, -1]
    def test_pattern_2(self):
        assert pattern(2) == [0, 0, 1, 1, 0, 0, -1, -1]
    def test_pattern_3(self):
        assert pattern(3) == [0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1]

class Test_phase:
    def test_phase_example_1(self):    
        actual = phase([1, 2, 3, 4, 5, 6, 7, 8])
        expected = [4, 8, 2, 2, 6, 1, 5, 8]
        assert actual == expected
    def test_phase_example_2(self):    
        actual = phase([4, 8, 2, 2, 6, 1, 5, 8])
        expected = [3, 4, 0, 4, 0, 4, 3, 8]
        assert actual == expected
    def test_phase_example_3(self):    
        actual = phase([3, 4, 0, 4, 0, 4, 3, 8])
        expected = [0, 3, 4, 1, 5, 5, 1, 8]
        assert actual == expected
    def test_phase_example_4(self):    
        actual = phase([0, 3, 4, 1, 5, 5, 1, 8])
        expected = [0, 1, 0, 2, 9, 4, 9, 8]
        assert actual == expected

class Test_Larger:
    def test_part_one_example_1(self):    
        assert part_one('80871224585914546619083218645595') == '24176176'
    def test_part_one_example_2(self):    
        assert part_one('19617804207202209144916044189917') == '73745418'
    def test_part_one_example_3(self):    
        assert part_one('69317163492948606335995924319873') == '52432133'

class Test_Part_Two:
    def test_part_two_example_1(self):
        assert part_two('03036732577212944063491565474664') == '84462026'
    def test_part_two_example_2(self):
        assert part_two('02935109699940807407585447034323') == '78725270'
    def test_part_two_example_3(self):
        assert part_two('03081770884921959731165446850517') == '53553731'

