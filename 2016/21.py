"""Day 21: Scrambled Letters and Hash."""
import fileinput
import re
from collections import deque

rotate_based_lookup: dict[int,int] = { }
rotate_based_lookup_rev: dict[int,int] = { }

for i in range(8):
    steps = 1 + i
    if i >= 4:
        steps += 1
    final_pos = (i + steps) % 8
    rotate_based_lookup[i] = steps
    rotate_based_lookup_rev[final_pos] = -steps

def rotate(passwd: list[str], steps: int) -> list[str]:
    p = deque(passwd)
    p.rotate(steps)
    return list(p)


SWAP_POSITION = re.compile(r'swap position (\d+) with position (\d+)')
SWAP_LETTER = re.compile(r'swap letter (\w) with letter (\w)')
ROTATE_LEFT = re.compile(r'rotate left (\d+) step')
ROTATE_RIGHT = re.compile(r'rotate right (\d+) step')
REVERSE_POSITIONS = re.compile(r'reverse positions (\d+) through (\d+)')
MOVE_POSITION = re.compile(r'move position (\d+) to position (\d+)')
ROTATE_BASED = re.compile(r'rotate based on position of letter (\w)')

def part_one(instructions: list[str], password: str = 'abcdefgh'):
    """Solution to part one."""
    passwd = list(password)
    

    for instruction in instructions:
       if (res := SWAP_POSITION.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue

       if (res := SWAP_LETTER.match(instruction)):
           x, y = res.groups()
           ix, iy = passwd.index(x), passwd.index(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue

       if (res := ROTATE_LEFT.match(instruction)):
           x = res.group(1)
           passwd = rotate(passwd, -int(x))
           continue

       if (res := ROTATE_RIGHT.match(instruction)):
           x = res.group(1)
           passwd = rotate(passwd, int(x))
           continue

       if (res := REVERSE_POSITIONS.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           passwd[ix:iy+1] = reversed(passwd[ix:iy+1])
           continue

       if (res := MOVE_POSITION.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           char = passwd.pop(ix)
           passwd.insert(iy, char)
           continue
    
       if (res := ROTATE_BASED.match(instruction)):
           x = res.group(1)
           ix = passwd.index(x)
           passwd = rotate(passwd, rotate_based_lookup[ix])
           continue

       print(f'Unknown instruction: {instruction}')


    return ''.join(passwd)


def part_two(instructions: list[str]):
    """Solution to part two."""
    passwd = list(scrambled := 'fbgdceah')


    # start from the last instruction and reverse each operation
    for instruction in reversed(instructions):

       # unmodified
       if (res := SWAP_POSITION.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue

       # unmodified
       if (res := SWAP_LETTER.match(instruction)):
           x, y = res.groups()
           ix, iy = passwd.index(x), passwd.index(y)
           passwd[ix], passwd[iy] = passwd[iy], passwd[ix]
           continue


       # reverse of rotate left is rotate right
       if (res := ROTATE_LEFT.match(instruction)):
           x = res.group(1)
           passwd = rotate(passwd, int(x))
           continue

       # reverse of rotate right is rotate left
       if (res := ROTATE_RIGHT.match(instruction)):
           x = res.group(1)
           passwd = rotate(passwd, -int(x))
           continue

       # unmodified
       if (res := REVERSE_POSITIONS.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           passwd[ix:iy+1] = reversed(passwd[ix:iy+1])
           continue

       # reverse of move position x to y is move position y to x
       if (res := MOVE_POSITION.match(instruction)):
           x, y = res.groups()
           ix, iy = int(x), int(y)
           char = passwd.pop(iy)
           passwd.insert(ix, char)
           continue
    
       # reverse of rotate based on position of letter x
       if (res := ROTATE_BASED.match(instruction)):
           x = res.group(1)
           ix = passwd.index(x)
           passwd = rotate(passwd, rotate_based_lookup_rev[ix])
           continue
    

    unscrambled = ''.join(passwd)
    p1_scrambled = part_one(instructions, password=unscrambled)
    assert p1_scrambled == scrambled, f'Part one re-scramble failed: {p1_scrambled} != {scrambled}'

    return unscrambled

def main():
    """Parse input file, pass to puzzle solvers."""
    instructions: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        instructions.append(line)

    print('part_one', part_one(instructions))

    print('part_two', part_two(instructions))


if __name__ == '__main__':
    main()
