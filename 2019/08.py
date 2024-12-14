"""Day 8: Space Image Format."""
import fileinput
from collections import Counter

def part_one(input:str):
    """Solution to part one."""
    l = 25*6
    zeros=l+1
    count:Counter[str]=Counter()
    while len(input):
        c = Counter(input[:l])
        if c['0'] < zeros:
            zeros = c['0']
            count = c
        input=input[l:]
    
    return int(count['1']) * int(count['2'])

def part_two(line:str):
    """Solution to part two."""
    l = 25*6
    layers:list[list[str]]=[]
    input=list(line)
    while len(input):
        layers.append(input[:l])
        input=input[l:]

    
    img:list[str] = []    
    for i in range(l):
        lenl = len(layers)
        print(lenl)
        for n in range(lenl):
            print(i,n, layers)
            p = layers[i][n]
            if p == '2':
                if n == lenl-1:
                    img.append(' ')
                continue
            if p == '0':
                img.append('.')
                break
            if p == '1':
                img.append('#')
                break
            
            
    while len(img):
        print(''.join(img[:25]))
        try:
            img=img[25:]
        except:
            break


    return 'todo'


def main():
    """Parse input file, pass to puzzle solvers."""
    input=""
    for line in fileinput.input():
        input += line.strip()


    print('part_one', part_one(input))

    print('part_two', part_two(input))


main()
