# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
from typing import List

def is_valid(passphrase:str) -> bool:
    words = passphrase.split(' ')
    return len(words) == len(set(words))

def sorted_letters(word:str) -> str:
    return ''.join(sorted(word))

def is_valid2(passphrase:str) -> bool:
    words = passphrase.split(' ')
    words = [sorted_letters(word) for word in words]
    return len(words) == len(set(words))


def part_one(passphrases: List[str]) -> int:
    """ part one """
    sum = 0
    for passphrase in passphrases:
        if is_valid(passphrase):
            sum += 1
    return sum


def part_two(passphrases: List[str]) -> int:
    """ part two """
    sum = 0
    for passphrase in passphrases:
        if is_valid2(passphrase):
            sum += 1
    return sum


def main():
    """ main """
    passphrases: List[str] = []
    for line in sys.stdin:
        line = line.replace('\n', '')
        passphrases.append(line)

    print('part_one', part_one(passphrases))

    print('part_two', part_two(passphrases))


main()
