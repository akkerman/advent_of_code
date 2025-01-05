"""Day 14: Space Stoichiometry."""
import fileinput
from collections import deque, defaultdict
import math

Chemical = tuple[int, str]
Reactions = dict[str, tuple[int, list[Chemical]]]


def get_ore_for_fuel(reactions: Reactions, fuel: int) -> int:
    """Calculate the amount of ORE needed to produce fuel."""
    queue: deque[Chemical] = deque()
    queue.append((fuel, 'FUEL'))

    surplus: dict[str, int] = defaultdict(int)

    ore = 0
    while queue:
        needed_quantity, needed_chemical = queue.popleft()
        if needed_chemical == 'ORE':
            ore += needed_quantity
            continue

        needed_quantity -= surplus[needed_chemical]
        surplus[needed_chemical] = 0

        yield_quantity, input_chemicals = reactions[needed_chemical]
        factor = math.ceil(needed_quantity / yield_quantity)
        surplus[needed_chemical] += factor * yield_quantity - needed_quantity
        queue.extend((q*factor, c) for q,c in input_chemicals)


    return ore

def part_one(reactions: Reactions) -> int:
    """Calculate the amount of ORE needed to produce 1 FUEL."""
    return get_ore_for_fuel(reactions, 1)


def part_two(reactions: Reactions) -> int:
    """Calculate the maximum amount of fuel that can be produced with 1 trillion ore."""
    max_ore = 1000000000000

    # binary search for the maximum amount of fuel that can be produced
    low = 1
    high = max_ore
    while low < high:
        mid = (low + high) // 2
        ore = get_ore_for_fuel(reactions, mid)
        if ore > max_ore:
            high = mid
        else:
            low = mid + 1

    return low - 1

def parse_chemical(chemical: str) -> Chemical:
    """Parse a chemical string into a tuple of (quantity, name)."""
    quantity, name = chemical.split(' ')
    return (int(quantity), name)

def parse_input(lines: list[str]) -> Reactions:
    """Parse input lines into data structures."""
    reactions: dict[str, tuple[int, list[Chemical]]] = {}
    for line in lines:
        inputs, output = line.split(' => ')
        inputs = inputs.split(', ')
        inputs = [parse_chemical(chem) for chem in inputs]
        output = parse_chemical(output)
        reactions[output[1]] = (output[0], inputs)

    assert len(reactions) == len(lines) # no reactions yield the same chemical
    return reactions

def main():
    """Parse input file, pass to puzzle solvers."""
    lines: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        lines.append(line)

    reactions = parse_input(lines)


    print('part_one', part_one(reactions))

    print('part_two', part_two(reactions))


if __name__ == '__main__':
    main()

def test_example_1():
    """Test against example from the puzzle."""
    lines = [
        '10 ORE => 10 A',
        '1 ORE => 1 B',
        '7 A, 1 B => 1 C',
        '7 A, 1 C => 1 D',
        '7 A, 1 D => 1 E',
        '7 A, 1 E => 1 FUEL',
    ]
    reactions = parse_input(lines)
    expected = 31
    actual = part_one(reactions)
    assert actual == expected

def test_example_2():
    """Test against example from the puzzle."""
    lines = [
        '9 ORE => 2 A',
        '8 ORE => 3 B',
        '7 ORE => 5 C',
        '3 A, 4 B => 1 AB',
        '5 B, 7 C => 1 BC',
        '4 C, 1 A => 1 CA',
        '2 AB, 3 BC, 4 CA => 1 FUEL',
    ]
    reactions = parse_input(lines)
    expected = 165
    actual = part_one(reactions)
    assert actual == expected

def test_example_3():
    """Test against example from the puzzle."""
    lines = [
        '157 ORE => 5 NZVS',
        '165 ORE => 6 DCFZ',
        '44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL',
        '12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ',
        '179 ORE => 7 PSHF',
        '177 ORE => 5 HKGWZ',
        '7 DCFZ, 7 PSHF => 2 XJWVT',
        '165 ORE => 2 GPVTF',
        '3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT',
    ]
    reactions = parse_input(lines)
    expected = 13312
    actual = part_one(reactions)
    assert actual == expected

def test_example_4():
    """Test against example from the puzzle."""
    lines = [
        '2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG',
        '17 NVRVD, 3 JNWZP => 8 VPVL',
        '53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL',
        '22 VJHF, 37 MNCFX => 5 FWMGM',
        '139 ORE => 4 NVRVD',
        '144 ORE => 7 JNWZP',
        '5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC',
        '5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV',
        '145 ORE => 6 MNCFX',
        '1 NVRVD => 8 CXFTF',
        '1 VJHF, 6 MNCFX => 4 RFSQX',
        '176 ORE => 6 VJHF',
    ]
    reactions = parse_input(lines)
    expected = 180697
    actual = part_one(reactions)
    assert actual == expected

def test_example_5():
    """Test against example from the puzzle."""
    lines = [
        '171 ORE => 8 CNZTR',
        '7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL',
        '114 ORE => 4 BHXH',
        '14 VRPVC => 6 BMBT',
        '6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL',
        '6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT',
        '15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW',
        '13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW',
        '5 BMBT => 4 WPTQ',
        '189 ORE => 9 KTJDG',
        '1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP',
        '12 VRPVC, 27 CNZTR => 2 XDBXC',
        '15 KTJDG, 12 BHXH => 5 XCVML',
        '3 BHXH, 2 VRPVC => 7 MZWV',
        '121 ORE => 7 VRPVC',
        '7 XCVML => 6 RJRHP',
        '5 BHXH, 4 VRPVC => 5 LTCX',
    ]
    reactions = parse_input(lines)
    expected = 2210736
    actual = part_one(reactions)
    assert actual == expected

def test_example_3_part2():
    """Test against example from the puzzle."""
    lines = [
        '157 ORE => 5 NZVS',
        '165 ORE => 6 DCFZ',
        '44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL',
        '12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ',
        '179 ORE => 7 PSHF',
        '177 ORE => 5 HKGWZ',
        '7 DCFZ, 7 PSHF => 2 XJWVT',
        '165 ORE => 2 GPVTF',
        '3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT',
    ]
    reactions = parse_input(lines)
    expected = 82892753
    actual = part_two(reactions)
    assert actual == expected

def test_example_4_part2():
    """Test against example from the puzzle."""
    lines = [
        '2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG',
        '17 NVRVD, 3 JNWZP => 8 VPVL',
        '53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL',
        '22 VJHF, 37 MNCFX => 5 FWMGM',
        '139 ORE => 4 NVRVD',
        '144 ORE => 7 JNWZP',
        '5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC',
        '5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV',
        '145 ORE => 6 MNCFX',
        '1 NVRVD => 8 CXFTF',
        '1 VJHF, 6 MNCFX => 4 RFSQX',
        '176 ORE => 6 VJHF',
    ]
    reactions = parse_input(lines)
    expected = 5586022
    actual = part_two(reactions)
    assert actual == expected

def test_example_5_part2():
    """Test against example from the puzzle."""
    lines = [
        '171 ORE => 8 CNZTR',
        '7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL',
        '114 ORE => 4 BHXH',
        '14 VRPVC => 6 BMBT',
        '6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL',
        '6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT',
        '15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW',
        '13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW',
        '5 BMBT => 4 WPTQ',
        '189 ORE => 9 KTJDG',
        '1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP',
        '12 VRPVC, 27 CNZTR => 2 XDBXC',
        '15 KTJDG, 12 BHXH => 5 XCVML',
        '3 BHXH, 2 VRPVC => 7 MZWV',
        '121 ORE => 7 VRPVC',
        '7 XCVML => 6 RJRHP',
        '5 BHXH, 4 VRPVC => 5 LTCX',
    ]
    reactions = parse_input(lines)
    expected = 460664
    actual = part_two(reactions)
    assert actual == expected
