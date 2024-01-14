# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import hashlib


def md5(s:str)->str:
    m = hashlib.md5()
    m.update(s.encode())
    return m.hexdigest()


def part_one(door_id):
    """ part one """
    password = []

    i = 0
    while len(password) < 8:
        stuff = door_id + str(i)
        h = md5(stuff)
        if h[:5] == '00000':
            password.append(h[5])

        i+=1

    return "".join(password)



def part_two(door_id):
    """ part two """
    return 'todo'


def main():
    """ main """
    door_id = ""
    for line in sys.stdin:
        line = line.replace('\n', '')
        door_id = line
        break

    print('part_one', part_one(door_id))

    print('part_two', part_two(door_id))


main()
