import sys

# blijf als alles rondom vrij
#
# de _eerste_ richting wijzigt elke ronde Z N W E Z N W E etc.
#
# alleen als move niet mogelijk is probeert elf individueel de volgende richting
# maar begint dus weer met dezelfde richting als alle andere elfs de volgende ronde
#
# Vrij is als de naburige DRIE velden vrij zijn in die richting
# move Z ; try ZW Z ZE

def free_moves(elfs, current): 
    we, ns = current

    n = (we, ns-1)
    s = (we, ns+1)
    w = (we-1, ns)
    e = (we+1, ns)

    nw = (we-1, ns-1)
    ne = (we+1, ns-1)

    nw = (we-1, ns+1)
    ne = (we+1, ns+1)




def part_one(elfs):
    """ get number of free squares after 10 rounds """
    return 'todo'


def part_two(lines):
    """ part_two """
    return 'todo'



def elfs_as_coords(lines):
    """ translate the input to a set of coordinates """
    elfs = set()
    for i, line in enumerate(lines):
        for j, entry in enumerate(line):
            if entry == '#':
                elfs.add((i, j))

    return elfs


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        lines.append(line)

    elfs = elfs_as_coords(lines)

    print('part_one', part_one(elfs))

    print('part_two', part_two(lines))


main()
