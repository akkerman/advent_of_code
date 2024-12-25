"""Day 9: Sensor Boost."""
import fileinput
from collections import defaultdict
from intcode import computer, IO

class SensorBoost(IO):
    def __init__(self, value:int):
        self.value = value
        self.outputs = list[int]()

    def input(self) -> int:
        return self.value 

    def output(self, value: int):
        self.outputs.append(value)




def part_one(program:list[int]):
    """Produce the boost keycode."""
    sensorboost = SensorBoost(1)
    computer(defaultdict(int, enumerate(program)), sensorboost)
    return sensorboost.outputs[0]



def part_two(program:list[int]):
    """Produce the coordinates of the distress signal."""
    sensorboost = SensorBoost(2)
    computer(defaultdict(int, enumerate(program)), sensorboost)
    return sensorboost.outputs[0]


def main():
    """Parse input file, pass to puzzle solvers."""
    program: list[int] = []
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))

    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
