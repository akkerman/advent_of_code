import sys
lines = []

for line in sys.stdin:
    line = line.replace('\n', '')
    lines.append(line)

def parse_instructions(input):
    return [(s[0],int(s[1:])) for s in input.split(', ')]

instructions=parse_instructions(lines[0])

compas="NESW"

def part_one():
    direction = 0 # 'N'
    blocks = { 'N':0, 'E':0, 'S':0, 'W':0 }

    for turn, num_blocks in instructions:
        direction += (1 if turn == 'R' else -1)
        direction = direction % 4 
        blocks[compas[direction]]+=num_blocks

    return abs(blocks['N']-blocks['S']) + abs(blocks['E']-blocks['W'])

def part_two():
    direction = 0 # 'N'
    loc = (0,0)
    locations = set()
    locations.add(loc)

    for (turn, num_blocks) in instructions:
        direction += (1 if turn == 'R' else -1)
        direction = direction % 4 

        for _ in range(num_blocks):
            d = compas[direction]
            n,e = loc
            if d == 'N': loc = (n+1, e)
            if d == 'S': loc = (n-1, e)
            if d == 'E': loc = (n,   e+1)
            if d == 'W': loc = (n,   e-1)

            if loc in locations:
                n,e = loc
                return abs(n) + abs(e)

            locations.add(loc)




print('part one', part_one()) 
# count how many blocks we need to navigate in each direction of a compass
# The directions are in an array we can point to the direction with a number
# to connect the end and begin of the array we take the modulo

print('part two', part_two())
# At first I was jumping from one position to another
# The solution is to generate all positions in between.
# Since the final location is known, we can calculate based on that
