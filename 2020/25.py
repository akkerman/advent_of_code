# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


# Card ##
# Public key : 5764801
# Loop size  : 8

# Door ##
# Public key : 17807724
# Loop size  : 11

def transform(loop):
    subj = 7
    pub = 1
    for _ in range(0,loop):
        pub = pub * subj 
        pub = pub % 20201227
    return pub

def crack_loops(pub_keys):
    loops = [0,0]

    loop_size = 1
    subj = 7
    pub = 1
    while loops[0] == 0 or loops[1] == 0:
        pub = pub * subj 
        pub = pub % 20201227

        for i in range(0,2):
            if pub == pub_keys[i] and loops[i] == 0:
                print('cracked loop', i, loop_size)
                loops[i] = loop_size

        loop_size += 1

    return loops

def part_one(lines):
    """ part one """

    print(crack_loops(lines))

    return 'todo'


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(int(line))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
