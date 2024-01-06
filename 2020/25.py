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

def transform(loop, subj = 7):
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

def part_one(keys):
    """ part one """

    
    loops  = crack_loops(keys)

    enc_key=transform(loops[0], keys[1])
    print(f'transform({loops[0]}, {keys[1]})={enc_key}')
    enc_key=transform(loops[1], keys[0])
    print(f'transform({loops[1]}, {keys[0]})={enc_key}')



def main():
    """ main """
    keys = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        keys.append(int(line))

    part_one(keys)


main()
