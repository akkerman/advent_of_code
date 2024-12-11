"""Day 5: Alchemical Reduction."""
import fileinput
import string

def react(polymer:str, start:int=0):
    """Perform one pass of polymer reaction."""
    for i in range(start, len(polymer)-1):
        a, b = polymer[i], polymer[i+1]
        if a != b and a.lower() == b.lower():
            return polymer[:i] + polymer[i+2:], i-1 if i > 0 else 0
    return polymer, -1

def part_one(polymer:str):
    """Determine length of polymer after performing all polymer recations."""
    start = 0
    while True:
        new_polymer, start = react(polymer, start)
        if start == -1:
            break
        polymer = new_polymer
    return len(polymer)


def part_two(polymer:str):
    """Find the shortest polymer after removing all units of one type and performing all polymer reactions."""
    def remove_unit(polymer:str, char:str):
        return polymer.replace(char, '').replace(char.upper(), '')

    return min(
        part_one(remove_unit(polymer, char))
        for char in string.ascii_lowercase
    )



def main():
    """Parse input file, pass to puzzle solvers."""
    polymer=''
    for line in fileinput.input():
        polymer = line.strip()
        break

    print('part_one', part_one(polymer))

    print('part_two', part_two(polymer))


main()
