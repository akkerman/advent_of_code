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

    print('generatating password for door 1...')

    i = 0
    while len(password) < 8:
        stuff = door_id + str(i)
        h = md5(stuff)
        if h[:5] == '00000':
            password.append(h[5])
            print("".join(password))

        i+=1

    return "".join(password)



def part_two(door_id):
    """ part two """
    password = ["_"] * 8
    print('generatating password for door 2...')

    i = 0
    while "_" in password:
        i+=1
        stuff = door_id + str(i)
        h = md5(stuff)
        if h[:5] == '00000':
            if h[5] not in '01234567':
                continue
            idx = int(h[5])
            if password[idx] != '_':
                continue
            password[idx] = h[6]
            print("".join(password))


    return "".join(password)


def main():
    """ main """
    door_id = ""
    for line in sys.stdin:
        line = line.replace('\n', '')
        door_id = line
        break

    print('part_one', part_one(door_id))
    print('')
    print('part_two', part_two(door_id))


main()
