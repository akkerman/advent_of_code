"""Day 21: Keypad Conundrum."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

Coord = tuple[int,int]


def create_pad(buttons: list[str]) -> dict[str, Coord]:
    pad: dict[str, tuple[int,int]] = dict()
    for row, btns in enumerate(buttons):
        for col, c in enumerate(btns):
            if c == ' ': continue
            pad[c] = (row,col)
    return pad

numpad: dict[str, Coord] = create_pad(['789', '456', '123', ' 0A'])
dirpad: dict[str, Coord] = create_pad([' ^A', '<v>'])

    

def keypresses_numpad(start:str, end:str)-> str:
    sr, sc = numpad[start]
    er, ec = numpad[end]

    dr = er - sr
    dc = ec - sc

    presses = ''
    if dr < 0 and sr == 3:
        presses += '^' * abs(dr)
    if dc > 0:
        presses += '>' * dc
    if dc < 0:
        presses += '<' * abs(dc)
    if dr < 0 and sr < 3:
        presses += '^' * abs(dr)
    if dr > 0:
        presses += 'v' * dr
    presses += 'A'
    return presses


def keypresses_dirpad(start:str, end:str)-> str:
    sr, sc = dirpad[start]
    er, ec = dirpad[end]

    dr = er - sr
    dc = ec - sc

    presses = ''
    if dr > 0:
        presses += 'v' * dr
    if dc > 0:
        presses += '>' * dc
    if dc < 0:
        presses += '<' * abs(dc)
    if dr < 0:
        presses += '^' * abs(dr)
    presses += 'A'
    return presses

def simulate(pad: dict[str, Coord], sequence: str):
    rev = {v:k for k,v in pad.items()}
    r,c = pad['A']
    simulation = ''
    for press in sequence:
        if press == '<':
            c -= 1
        elif press == '>':
            c += 1
        elif press == '^':
            r -= 1
        elif press == 'v':
            r += 1
        elif press == 'A':
            c=c
            r=r
        else:
            raise ValueError(f'Invalid press: {press}')

        element = rev[(r,c)]
        if press == 'A':
            # print(f'press: {element} at {r},{c}')
            simulation += element
        # else:
        #     print(f'goto : {element} at {r},{c}')
    return simulation




def make_presscode(keypresses: callable): # type: ignore
    def presscode(code: str)-> str:
        return "".join(keypresses(s,e) for s,e in zip('A'+code, code)) # type: ignore
    return presscode

presscode_numpad = make_presscode(keypresses_numpad)
presscode_dirpad = make_presscode(keypresses_dirpad)

def gen_code(code: str) -> str:
    presses = presscode_numpad(code)
    print(presses)
    presses = presscode_dirpad(presses)
    print(presses)
    presses = presscode_dirpad(presses)
    print(presses)
    return presses

def part_one(codes: list[str]):
    """Solution to part one."""
    total  = 0
    for code in codes:
        # print('\n'+code)
        presses = presscode_numpad(code)
        # print(presses)
        presses = presscode_dirpad(presses)
        # print(presses)
        presses = presscode_dirpad(presses)
        # print(presses)
        # print(len(presses) , int(code[:-1]))
        total += (len(presses) * int(code[:-1]))

    return total


def part_two(lines):
    """Solution to part two."""
    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    codes: list[str] = []
    for line in fileinput.input():
        line = line.strip()
        codes.append(line)

    # too low:  70128
    # nope:     212830
    # too high: 214358
    # too high: 350640
    print('part_one', part_one(codes))

    # code = '379A'
    # aoc_sim = '<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A'
    #
    # gen_code('379A')
    #
    # print('')
    #
    # sim = simulate(dirpad, aoc_sim)
    # print(sim)
    # sim = simulate(dirpad, sim)
    # print(sim)
    # code = simulate(numpad, sim)
    # print(code)


    
    print('part_two', part_two(codes))

if __name__ == '__main__':
    main()



def test_keypresses_numpad_01():
    # not via gap
    assert keypresses_numpad('0', '1') == '^<A'

def test_keypresses_dirpad_AR():
    assert keypresses_dirpad('A', '>') == 'vA'
def test_keypresses_dirpad_RL():
    assert keypresses_dirpad('<', '>') == '>>A'
def test_keypresses_dirpad_RA():
    assert keypresses_dirpad('>', 'A') == '^A'
def test_keypresses_dirpad_AL():
    # not via gap
    assert keypresses_dirpad('A', '<') == 'v<<A'
def test_keypresses_dirpad_LA():
    # not via gap
    assert keypresses_dirpad('<', 'A') == '>>^A'

def test_code_029A():
    assert(len(gen_code('029A'))) == 68

def test_code_980A():
    assert(len(gen_code('980A'))) == 60

def test_code_179A():
    assert(len(gen_code('179A'))) == 68

def test_code_456A():
    assert(len(gen_code('456A'))) == 64

def test_code_379A():
    assert(len(gen_code('379A'))) == 64


def test_simulate_numpad():
    assert(simulate(numpad, presscode_numpad('974A'))) == '974A'

def test_simulate_dirpad():
    assert(simulate(dirpad, presscode_dirpad('^^>A'))) == '^^>A'

def test_simulate():
    presses = '<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A'
    simulation = simulate(numpad, simulate(dirpad, simulate(dirpad, presses)))
    assert(simulation) == '379A'

def test_simulate_2():
    presses = presscode_dirpad(presscode_numpad('379A'))
    simulation = simulate(numpad, simulate(dirpad, presses))
    assert(simulation) == '379A'

def test_simulate_3():
    numcode_presses = presscode_numpad('379A')
    presses = presscode_dirpad(presscode_dirpad(numcode_presses))
    simulation = simulate(numpad, simulate(dirpad, simulate(dirpad, presses)))
    assert(simulation) == '379A'

    

    
