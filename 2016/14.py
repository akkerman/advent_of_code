# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import hashlib

def md5_normal(s:str)->str:
    m = hashlib.md5()
    m.update(s.encode())
    return m.hexdigest()

def md5_stretched(s:str)->str:
    for _ in range(2017):
        s = md5_normal(s)
    return s


def quint_from_triplet(h:str):
    for i in range(len(h)-2):
        if h[i] == h[i+1] and h[i] == h[i+2]:
            return h[i] * 5
    return None


def solve(salt:str, stretch:bool=False):
    lookup = set[tuple[str,int]]()
    key_idx = set[int]()

    idx = 0

    md5 = md5_stretched if stretch else md5_normal

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
        if idx > 25000:
            print('too many for example')
            break

def part_one(salt:str):
    """ part one """
    return solve(salt)

def part_two(salt:str):
    """ part two """
    return solve(salt, stretch=True)


def main():
    """ main """
    lines = list[str]()
    for line in sys.stdin:
        line = line.replace('\n', '')
        lines.append(line)

    salt = lines[0]
    print('salt', salt)
    print('part_one', part_one(lines[0]))
    print('part_two', part_two(lines[0]))

main()
