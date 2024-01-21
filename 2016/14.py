# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import hashlib

def md5(s:str)->str:
    m = hashlib.md5()
    m.update(s.encode())
    return m.hexdigest()

def quint_from_triplet(h):
    for i in range(len(h)-2):
        if h[i] == h[i+1] and h[i] == h[i+2]:
            return h[i] * 5
    return None



def part_one(salt):
    """ part one """
    print('salt', salt)
    lookup = set()
    key_idx = set()

    idx = 0
    while True:



        md5hash = md5(salt+ str(idx))
        matches = [i for t, i in lookup if i > (idx-1000) and t in md5hash]
        for m in matches:
            key_idx.add(m)

        if len(key_idx) >= 64:
            return sorted(key_idx)[63]

        quint = quint_from_triplet(md5hash)
        if quint:
            lookup.add((quint, idx))

        idx+=1

def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines[0]))


main()
