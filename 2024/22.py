"""Day 22: Monkey Market."""
import fileinput
import heapq
import re
from collections import deque, defaultdict, Counter
from functools import lru_cache
from utils import perf_timer

def mix(secret:int, value:int) -> int:
    """Mix the secret number."""
    return secret ^ value
def prune(secret:int) -> int:
    """Prune the secret number."""
    return secret % 16777216

def next_secret(secret:int) -> int:
    """Generate the next secret number."""
    new_secret = secret
    new_secret = prune(mix(new_secret, new_secret * 64))
    new_secret = prune(mix(new_secret, new_secret // 32))
    new_secret = prune(mix(new_secret, new_secret * 2048))
    return new_secret

# def next_secret(secret:int) -> int:
#     """Generate the next secret number."""
#     secret = (secret ^ (secret << 6)) % 20201227
#     secret = (secret ^ (secret >> 5)) % 20201227
#     secret = (secret ^ (secret << 11)) % 20201227
#     return secret

def next_secret_2000(secret:int) -> int:
    """Generate the 2000th secret number."""
    for _ in range(2000):
        secret = next_secret(secret)
    return secret


def part_one(secrets:list[int]) -> int:
    """Solution to part one."""
    return sum(next_secret_2000(secret) for secret in secrets)


def part_two(secrets:list[int]) -> int:
    """Solution to part two."""
    return -1


def main():
    """Parse input file, pass to puzzle solvers."""
    secrets:list[int] = []
    for line in fileinput.input():
        line = int(line.strip())
        secrets.append(line)


    print('part_one', part_one(secrets))

    print('part_two', part_two(secrets))


if __name__ == '__main__':
    main()

def test_next_secret():
    assert next_secret(123) == 15887950
    assert next_secret(15887950) == 16495136 
    assert next_secret(16495136) == 527345 
    assert next_secret(527345) == 704524 
    assert next_secret(704524) == 1553684 
    assert next_secret(1553684) == 12683156 
    assert next_secret(12683156) == 11100544 
    assert next_secret(11100544) == 12249484 
    assert next_secret(12249484) == 7753432 
    assert next_secret(7753432) == 5908254 



def test_next_secret_2000():
    assert next_secret_2000(1) == 8685429
    assert next_secret_2000(10) == 4700978
    assert next_secret_2000(100) == 15273692
    assert next_secret_2000(2024) == 8667524
