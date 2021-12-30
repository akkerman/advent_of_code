import sys

lines = []
for line in sys.stdin:
    line = line.replace('\n', '')
    lines.append([int(s) for s in line])

maxRow = len(lines)
maxCol = len(lines[0])

coordinates = [(r,c) for r in range(0,maxRow) for c in range(0,maxCol)]

def pp(lines):
  for line in lines: 
      print(" ".join(map(str, line)))

def range_arround(i): 
    return range(i-1, i+2)

def withinCavern(r,c):
    return 0<=r and 0<=c and r<maxRow and c<maxCol

def energy_level(coords):
    (r,c) = coords
    return lines[r][c]

def get_neigbours(row,col):
    return [(r,c) for r in range_arround(row) for c in range_arround(col) 
            if withinCavern(r,c) and not (r,c) == (row,col)]

def next_step(step):
    return list(map(lambda line: list(map(lambda x: x+1, line)), step))

def flashers_left(step):
    for (row,col) in coordinates:
        if 9<step[row][col]:
            return True
    return False

def flash(step):
    for (row,col) in coordinates:
        val = step[row][col]
        if val <= 9:
            continue
        step[row][col]=0
        for (r,c) in get_neigbours(row,col):
            if 0 < step[r][c]:
              step[r][c]+=1

    if not flashers_left(step):
        return step
    return flash(step)

def part_one(lines):
    total = 0
    step = lines
    for _ in range(0,100):
       step = flash(next_step(step))
       flashers = [f for line in step for f in line if f == 0]
       total += len(flashers)
    return total

def part_two(lines):
    step = lines
    for nr in range(1,1000):
       step = flash(next_step(step))
       flashers = [f for line in step for f in line if f == 0]
       if len(flashers) == 100:
           return nr
    return -1

print(part_one(lines)) #  1601
print(part_two(lines)) #   368
