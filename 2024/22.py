"""Day 22: Monkey Market."""
import fileinput

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

def next_secret_2000(secret:int) -> int:
    """Generate the 2000th secret number."""
    for _ in range(2000):
        secret = next_secret(secret)
    return secret


def part_one(secrets:list[int]) -> int:
    """Solution to part one."""
    return sum(next_secret_2000(secret) for secret in secrets)

def last_digit(secret:int) -> int:
    """Last digit of the secret."""
    return int(str(secret)[-1])

DiffSequence = tuple[int, int, int, int]
LookupTable = dict[DiffSequence, int]

def sequences(secret:int) -> LookupTable:
    """Sequences of four differences and its corresponding price."""
    diffs:list[int] = []
    lookup: LookupTable = {}
    prev = last_digit(secret)
    for _ in range(2000):
        secret = next_secret(secret)
        last = last_digit(secret)
        diffs.append(last - prev)
        if len(diffs) >= 4:
            seq = tuple(diffs[-4:])
            if seq not in lookup:
                lookup[seq] = last # type: ignore
        prev = last
    return lookup

def part_two(secrets:list[int]) -> tuple[int, DiffSequence]:
    """Solution to part two."""
    lookup_tables: list[LookupTable]  = [sequences(secret) for secret in secrets]
    seqs = set(lookup_tables[0].keys()).union(*[lookup.keys() for lookup in lookup_tables])


    max_bananas = 0
    seq_for_monkey = (0,0,0,0)
    for seq in seqs:
        bananas = 0
        for lookup in lookup_tables:
            if seq in lookup:
                bananas += lookup[seq]
        if bananas > max_bananas:
            max_bananas = bananas
            seq_for_monkey = seq


    return max_bananas, seq_for_monkey


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

def test_last_num():
    assert last_digit(123) == 3
    assert last_digit(15887950) == 0
    assert last_digit(16495136) == 6

def test_next_secret_2000():
    assert next_secret_2000(1) == 8685429
    assert next_secret_2000(10) == 4700978
    assert next_secret_2000(100) == 15273692
    assert next_secret_2000(2024) == 8667524

def test_sequence():
    seq = sequences(123)
    assert seq[(-1, -1, 0, 2)] == 6

def test_seq_example():
    expected = (-2,1,-1,3)
    assert expected in sequences(1)
    assert expected in sequences(2)
    assert expected not in sequences(3)
    assert expected in sequences(2024)

def test_part_two():
    assert part_two([1,2,3,2024]) == (23, (-2,1,-1,3))


