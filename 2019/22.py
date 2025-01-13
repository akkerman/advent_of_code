"""Day 22: Slam Shuffle."""
import fileinput
import re

def deal_with_increment(deck: list[int], inc: int):
    """Deal with increment."""
    new_deck = deck.copy()
    new_deck = [0] * len(deck)
    for i, card in enumerate(deck):
        new_deck[(i * inc) % len(deck)] = card
    return new_deck

NUMBER = re.compile(r'-?\d+')

def shuffle(deck:list[int], instructions: list[str]):
    """Shuffle deck according to instructions."""

    new_deck = deck.copy()

    for instruction in instructions:
        if 'deal into new stack' in instruction:
            new_deck = new_deck[::-1]
            continue

        num = int(re.findall(NUMBER, instruction)[0])

        if 'deal with increment' in instruction:
            new_deck = deal_with_increment(new_deck, num)
            continue

        if 'cut' in instruction:
            new_deck = new_deck[num:] + new_deck[:num]
            continue

    return new_deck

def prev_position(instructions: list[str], length: int, position: int) -> int:
    """Return the position of a card after shuffling the deck."""
    for instruction in instructions[::-1]:
        if 'deal into new stack' in instruction:
            position = length - position - 1
            continue

        num = int(re.findall(NUMBER, instruction)[0])

        if 'cut' in instruction:
            position = (position + num) % length
            continue

        if 'deal with increment' in instruction:
            position = (length - ((position * num) % length)) % length
            continue

    return position


def part_one(lines: list[str]) -> int:
    """Solution to part one."""
    new_deck = shuffle(list(range(10007)), lines)
    return new_deck.index(2019)



def part_two(lines: list[str]) -> int:
    """Solution to part two."""
    pos = 2020
    for _ in range(101741582076661):
        pos = prev_position(lines, 119315717514047, pos)

    return pos

def main():
    """Parse input file, pass to puzzle solvers."""
    lines = list[str]()
    for line in fileinput.input():
        line = line.strip()
        lines.append(line)

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


if __name__ == '__main__':
    main()


import pytest
class Test_Shuffle():
    def test_shuffle_1(self):
        deck = list(range(10))
        instructions = [
            'deal with increment 7',
            'deal into new stack',
            'deal into new stack'
        ]
        assert shuffle(deck, instructions) == [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]

    def test_shuffle_2(self):
        deck = list(range(10))
        instructions = [
            'cut 6',
            'deal with increment 7',
            'deal into new stack'
        ]
        assert shuffle(deck, instructions) == [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]

    def test_shuffle_3(self):
        deck = list(range(10))
        instructions = [
            'deal with increment 7',
            'deal with increment 9',
            'cut -2'
        ]
        assert shuffle(deck, instructions) == [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]

    def test_shuffle_4(self):
        deck = list(range(10))
        instructions = [
            'deal into new stack',
            'cut -2',
            'deal with increment 7',
            'cut 8',
            'cut -4',
            'deal with increment 7',
            'cut 3',
            'deal with increment 9',
            'deal with increment 3',
            'cut -1'
        ]
        assert shuffle(deck, instructions) == [9,2,5,8,1,4,7,0,3,6]


class Test_Previous_Position_deal:
    @classmethod
    def setup_class(cls):
        cls.instructions = [ 'deal into new stack' ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_cut_pos():
    @classmethod
    def setup_class(cls):
        cls.instructions = [ 'cut 3' ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_cut_neg():
    @classmethod
    def setup_class(cls):
        cls.instructions = [ 'cut -2' ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_cut_negpos():
    @classmethod
    def setup_class(cls):
        cls.instructions = [ 'cut -2', 'cut 6' ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_increment():
    @classmethod
    def setup_class(cls):
        cls.instructions = [ 'deal with increment 7' ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_three():
    @classmethod
    def setup_class(cls):
        cls.instructions = [
            'deal with increment 7',
            'deal into new stack',
            'deal into new stack'
        ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]

class Test_Previous_Position_ten():
    @classmethod
    def setup_class(cls):
        cls.instructions = [
            'deal into new stack',
            'cut -2',
            'deal with increment 7',
            'cut 8',
            'cut -4',
            'deal with increment 7',
            'cut 3',
            'deal with increment 9',
            'deal with increment 3',
            'cut -1',
        ]
        cls.deck = shuffle(list(range(10)), cls.instructions)

    @pytest.mark.parametrize('position', [0,1,2,3,4,5,6,7,8,9])
    def test(self, position:int):
        assert prev_position(self.instructions, 10, position) == self.deck[position]
#
# class Test_Part_One_Position():
#     def test(self):
#         instructions = [
#             'deal with increment 7',
#             'deal into new stack',
#             'deal into new stack'
#             'cut 2',
#             'cut -2',
#         ]
#         pos = part_one(instructions)
#         assert prev_position(instructions, 10007, 2019) == pos
