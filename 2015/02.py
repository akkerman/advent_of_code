# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import string


def square_feet(x,y,z):
    sq = [x*y, y*z, z*x]
    return min(sq) + sum(2*d for d in sq)

def ribbon_length(x,y,z):
    return x*y*z + 2 * sum(sorted([x,y,z])[0:-1])


def part_one(dimensions):
    """ part one """
    return sum(square_feet(*d) for d in dimensions)


def part_two(dimensions):
    """ part two """
    return sum(ribbon_length(*d) for d in dimensions)


def main():
    """ main """
    dimensions = []
    for line in sys.stdin:
        line = line.replace('\n', '')

        nums = [int(i) for i in line.split('x')]
    
        dimensions.append(nums)

    print('part_one', part_one(dimensions))

    print('part_two', part_two(dimensions))


main()
