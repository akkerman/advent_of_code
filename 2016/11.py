# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re
from itertools import combinations
from collections import deque

def valid_floor(floor):
    # suppose type = 'chip'
    # if the corresponding generator is found state could be valid
    # if it is not found it can only be valid if no other generator is on the floor
    for name in [tup[0] for tup in floor if tup[1] == 'chip']:
        if (name, 'generator') in floor:
            continue
        if len([1 for n,t in floor if n!=name and t == 'generator']) > 0:
            return False

    return True

def floor_to_state(floor):
    chips = { tup for tup in floor if tup[1] == 'chip' }
    generators = floor - chips
    pairs = { (c, g) for c in chips for g in generators if c[1] == g[1] }
    return (len(pairs), len(chips), len(generators))

def to_state(floors, elevator):
    st = tuple([elevator])
    for floor in floors:
        st += floor_to_state(floor)
    return st


def candidate_pairs_to_move(floor):
    pairs = []
    for pair in combinations(floor, 2):
        pair = set(pair)
        if valid_floor(floor-pair):
            pairs.append(pair)
    return pairs

def candidate_singles_to_move(floor):
    singles = []
    for s in floor:
        single = set()
        single.add(s)
        if valid_floor(floor-single):
            singles.append(single)
    return singles

def print_floors(floors, elevator):
    for i in range(3,-1, -1):
        e = 'E' if elevator == i else ' '
        print('F'+str(i), e, floors[i])
    print('')

depth  = 0
empty = [set(), set(), set()]

def solve(starting_floors):
    visited = set()
    q = deque()
    q.append((0, starting_floors.copy(), 0))

    while True:
        steps, floors, elevator = q.popleft()

        if floors[:3] == empty:
            return steps

        state = to_state(floors, elevator)
        if state in visited:
            continue
        visited.add(state)

        singles = candidate_singles_to_move(floors[elevator])

        if elevator < 3:
            pairs = candidate_pairs_to_move(floors[elevator])
            next_floor = elevator+1
            for pair in pairs:
                # print('candidate', pair)
                if valid_floor(floors[next_floor] | pair):
                    fs = floors.copy()
                    # print('moving pair', pair, 'up to floor', next_floor)
                    fs[elevator] = fs[elevator] - pair
                    fs[elevator+1] = fs[next_floor] | pair
                    q.append((steps + 1, fs, next_floor))


            for single in singles:
                # print('candidate', single)
                if valid_floor(floors[next_floor] | single):
                    fs = floors.copy()
                    # print('moving single', single, 'up to floor', next_floor)
                    fs[elevator] = fs[elevator] - single
                    fs[next_floor] = fs[next_floor] | single
                    q.append((steps + 1, fs, next_floor))

        if elevator > 0 and floors[:elevator] != empty[:elevator]:

            next_floor = elevator-1
            for single in singles:
                # print('candidate', single)
                if valid_floor(floors[next_floor] | single):
                    fs = floors.copy()
                    # print('moving', single, 'down to floor', next_floor)
                    fs[elevator] = fs[elevator] - single
                    fs[next_floor] = fs[next_floor] | single
                    q.append((steps + 1, fs, next_floor))

def part_one(floors):
    """ part one """
    return solve(floors)



def part_two(floors):
    """ part two """
    f1 = floors[0]
    f1.add(('elerium','generator'))
    f1.add(('elerium','chip'))
    f1.add(('dilithium','generator'))
    f1.add(('dilithium','chip'))

    return solve(floors)

re_chip = re.compile('([^ ]+)-compatible')
re_rtg = re.compile('([^ ]+) generator')
def main():
    """ main """

    floors = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        floor = set()

        for m in re_chip.finditer(line):
            floor.add((m.group(1), 'chip'))


        for m in re_rtg.finditer(line):
            floor.add((m.group(1), 'generator'))


        floors.append(floor)

    print('part_one', part_one(floors))

    print('part_two', part_two(floors))


main()
