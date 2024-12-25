"""Day 13: Care Package."""
import fileinput
from collections import defaultdict, Counter
from intcode import computer, IO

Coord = tuple[int, int]

tiles = [' ', '#',  '█', '_', 'O']

BALL = 4
PADDLE = 3

class Arcade(IO):
    def __init__(self):
        self.screen: dict[Coord, str] = defaultdict(lambda: ' ')
        self.params: list[int] = []
        self.score = 0
        self.frame = 0
        self.ball = (0,0)
        self.paddle = (0,0)
        self.playing = False

    def input(self):
        return self.joystick()

    def output(self, value:int):
        self.draw(value)

    def draw(self, value:int):
        self.params.append(value)
        if len(self.params) == 3:
            x, y, tile = self.params
            if x == -1 and y == 0:
                self.score = tile
                self.params = []
                return
            if tile == BALL:
                self.ball = (x,y)
            if tile == PADDLE:
                self.paddle = (x,y)
            
            self.screen[(x,y)] = tiles[tile]
            self.params = []
            # if self.playing:
            #     self.print_screen()
            #     time.sleep(0.1)

    def print_screen(self):
        print("\033[H\033[J", end="")
        self.frame+=1
        print('Score:', self.score, 'Frame:', self.frame)
        for y in range(0, 23):
            for x in range(0, 40):
                print(self.screen[(x,y)], end='')
            print()
        print()

    def joystick(self):
        self.playing = True
        if self.ball[0] < self.paddle[0]:
            return -1
        elif self.ball[0] > self.paddle[0]:
            return 1
        else:
            return 0
        

def part_one(program: list[int]) -> int:
    """Solution to part one."""
    arcade = Arcade()


    computer(defaultdict(int, enumerate(program)), arcade)
    return Counter(arcade.screen.values())['█']


def part_two(program: list[int]) -> int:
    """Solution to part two."""
    arcade = Arcade()

    program[0] = 2
    computer(defaultdict(int, enumerate(program)), arcade)
    return arcade.score


def main():
    """Parse input file, pass to puzzle solvers."""
    program: list[int] = []
    for line in fileinput.input():
        program = list(map(int, line.strip().split(',')))

    print('part_one', part_one(program))

    print('part_two', part_two(program))


if __name__ == '__main__':
    main()
