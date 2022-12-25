# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string

keypad1 = [
    [None , None , None , None , None] ,
    [None , '1'  , '2'  , '3'  , None] ,
    [None , '4'  , '5'  , '6'  , None] ,
    [None , '7'  , '8'  , '9'  , None] ,
    [None , None , None , None , None] ,
]

keypad2 = [
[ None , None , None , None , None , None , None] ,
[ None , None , None , '1'  , None , None , None] ,
[ None , None , '2'  , '3'  , '4'  , None , None] ,
[ None , '5'  , '6'  , '7'  , '8'  , '9'  , None] ,
[ None , None , 'A'  , 'B'  , 'C'  , None , None] ,
[ None , None , None , 'D'  , None , None , None] ,
[ None , None , None , None , None , None , None] ,
]

def solve(lines, keypad, r, c):
    """ part one """
    code = ''
    for line in lines:
        for d in line:
            nr = r
            nc = c
            if d == 'U':
                nr -= 1
            if d == 'D':
                nr += 1
            if d == 'R':
                nc += 1
            if d == 'L':
                nc -= 1
            if keypad[nr][nc]:
                r, c = nr, nc

        code += keypad[r][c]
    return code


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', solve(lines, keypad1, 2, 2))

    print('part_two', solve(lines, keypad2, 3, 1))


main()
