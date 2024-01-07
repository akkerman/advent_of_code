# pylint: disable=missing-module-docstring,missing-function-docstring
# pylint: disable=invalid-name
import sys
import re


def create_calc_kms(target):
    def calc(speed, duration, rest):
        step = duration + rest
        remain = target % step
        div = (target - remain) // step
        if remain < duration:
            # reindeer is moving
            return speed * (duration * div + remain)
        # reindeer is resting
        return speed * duration * (1 + div)


    return calc

def part_one(lines):
    """ part one """
    calc = create_calc_kms(2503)
    return max(calc(*t) for t in lines)


def part_two(lines):
    """ part two """
    points = [0] * len(lines)
    for seconds in range(1, 2503+1):
        calc = create_calc_kms(seconds)
        dist = [calc(*t) for t in lines]
        m = max(dist)
        for i,n in enumerate(dist):
            if n == m:
                points[i]+=1
    return max(points)


def main():
    """ main """
    lines = []
    reg = re.compile(r'.+can.+ (\d+) .+ for (\d+) .+ (\d+) seconds.')
    for line in sys.stdin:
        line = line.replace('\n', '')

        speed, duration, rest = reg.match(line).groups()
        lines.append((int(speed), int(duration), int(rest)))

    print('part_one', part_one(lines))

    print('part_two', part_two(lines))


main()
