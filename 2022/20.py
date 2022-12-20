import sys

def part_one(lines, zero):
    file = lines[:]
    length = len(lines)

    for elem in lines:
        _, num = elem
        pos = file.index(elem)
        pos_new = (pos+num) % (length - 1)
        del file[pos]
        file.insert(pos_new, elem)

    pos = file.index(zero)
    _, x = file[(pos + 1000) % length]
    _, y = file[(pos + 2000) % length]
    _, z = file[(pos + 3000) % length]

    return x + y + z

def part_two(lines):
    pass


def main():
    idx = 1
    lines = []
    zero = None
    for line in sys.stdin:
        line = line.replace('\n', '')
        value = int(line)
        record = (idx, value)
        lines.append(record)
        idx += 1
        if value == 0:
            zero = record

    print('part_one', part_one(lines, zero))

    print('part_two', part_two(lines))


main()
