import sys
lines = []
data=''
for line in sys.stdin:
    line = line.replace('\n', '')
    lines.append(line)
    data=lines[0]

def part_one():
    return data.count('(') - data.count(')')

def part_two():
    count=0
    floor=0
    for c in data:
        count+=1
        if c == '(':
            floor+=1
        if c == ')':
            floor-=1
        if floor == -1:
            return count

print(part_one())
print(part_two())
