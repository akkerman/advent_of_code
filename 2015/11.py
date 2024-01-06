# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


alpha = 'abcdefghijklmnopqrstuvwxyz'
forbidden='iol'
def is_allowed(a,b,c):
    return a not in forbidden and b not in forbidden and c not in forbidden
valid_triples = { a+b+c for a,b,c in zip(alpha, alpha[1:], alpha[2:]) if is_allowed(a,b,c) }
valid_pairs = { a+a for a in alpha if a not in forbidden }
valid_alpha = [a for a in alpha if a not in forbidden]

def next_password(password):
    if password == []:
        return []
    last = password[-1]
    init = password[:-1]
    if last == 'z':
        return next_password(init) + 'a'

    if last in valid_alpha:
        char = valid_alpha[1+valid_alpha.index(last)]
    else:
        char = alpha[1+alpha.index(last)]

    return init + char 

def is_valid(password):
    for c in forbidden:
        if c in password:
            return False
    
    triples = { a+b+c for a,b,c in zip(password, password[1:], password[2:]) }
    if len(triples & valid_triples) == 0:
        return False

    pairs = { a+b for a,b in zip(password,password[1:]) }
    if len(pairs & valid_pairs) < 2:
        return False

    return True

def next_valid_password(password):
    p = next_password(password)
    while not is_valid(p):
        p = next_password(p)
    return p


assert is_valid('hijklmmn') == False
assert is_valid('abbceffg') == False
assert is_valid('abbcegjk') == False
assert is_valid('abcdffaa') == True
assert is_valid('ghjaabcc') == True
# assert next_valid_password('abcdefgh') == 'abcdffaa'
# assert next_valid_password('ghijklmn') == 'ghjaabcc'

def part_one(password):
    """ part one """
    return next_valid_password(password)


def part_two(lines):
    """ part two """
    return 'todo'


def main():
    """ main """
    lines = []
    for line in sys.stdin:
        line = line.replace('\n', '')
    
        lines.append(line)

    print('part_one', part_one(lines[0]))

    print('part_two', part_two(lines))


main()
