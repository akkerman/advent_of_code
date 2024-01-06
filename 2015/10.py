# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def play(sequence):
    cur = sequence[0]
    c = 1
    new=[]
    for n in sequence[1:]:
        if n == cur:
            c+=1
        else:
            new.append(str(c))
            new.append(cur)
            cur=n
            c=1

    new.append(str(c))
    new.append(cur)
    return new
            

def part_one(sequence):
    """ part one """
    for _ in range(0,40):
        sequence = play(sequence)
    return len(sequence), sequence


def part_two(sequence): # input from part_one
    """ part two """
    for _ in range(0,10):
        sequence = play(sequence)
    return len(sequence)


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    p1, seq = part_one(list(lines[0]))
    print('part_one', p1)

    print('part_two', part_two(seq))


main()
