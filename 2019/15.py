"""Day 15: Oxygen System."""
import fileinput
import heapq
from collections import defaultdict
import random
from utils import perf_timer

NORTH = 1
SOUTH = 2
WEST = 3
EAST = 4

UNEXPLORED = -1
WALL = 0
MOVED = 1
FOUND = 2

Coord = tuple[int,int]

wind2dir = {
    NORTH: (0,1),
    SOUTH: (0,-1),
    WEST: (-1,0),
    EAST: (1,0)
}

class Droid:
    def __init__(self):
        self.current_dir = NORTH
        self.current_pos = (0,0)
        self.maze = defaultdict[Coord, int](lambda: UNEXPLORED)
        self.oxygen_pos: Coord = (0,0)
        self.maze[self.current_pos] = MOVED

    def move(self) -> int:
        """Don't move."""
        return 0

    def status(self, value:int) -> None:
        """Don't do anything."""
        value = value


    def next_pos(self, dir:int=0) -> Coord:
        """Determine the next position based on the current direction."""
        x,y = self.current_pos
        dx,dy = wind2dir[dir] if dir else wind2dir[self.current_dir]
        return (x+dx, y+dy)

    def print_maze(self):
        """Print ascii representation of the maze."""
        min_x = min(x for x,_ in self.maze.keys())
        max_x = max(x for x,_ in self.maze.keys())
        min_y = min(y for _,y in self.maze.keys())
        max_y = max(y for _,y in self.maze.keys())


        for y in range(max_y, min_y-1, -1):
            for x in range(min_x, max_x+1):
                status = self.maze[(x,y)]
                if (x,y) == (0,0):
                    print('D', end='')
                elif (x,y) == self.oxygen_pos or status == FOUND:
                    print('O', end='')
                elif status == UNEXPLORED:
                    print('?', end='')
                elif status == WALL:
                    # print('#', end='')
                    print('█', end='')
                elif status == MOVED:
                    print('.', end='')
                else:
                    raise ValueError('Unknown status {}'.format(status))
            print()

class DrunkenDroid(Droid):
    """Droid that moves randomly."""

    def move(self) -> int:
        """Move the droid in a random direction."""
        if len(self.maze.keys()) >= 1659:
            return 0

        self.current_dir = random.choice([1,2,3,4])
        return self.current_dir

    def status(self, value:int) -> None:
        """Record the status of the droid's move."""
        if value == WALL:
            self.maze[self.next_pos()] = WALL
            return

        self.maze[self.current_pos] = value
        self.current_pos = self.next_pos()

        if value == FOUND and self.oxygen_pos == (0,0):
            # print('Found oxygen system at', self.current_pos)
            self.oxygen_pos = self.current_pos

class DiscoveringDroid(Droid):
    DISCOVERING = 0
    BACKTRACKING = 1

    def __init__(self):
        super().__init__()
        self.unexplored = set[Coord]()
        self.path = list[Coord]()
        self.unexplored.update(self.unexplored_neighbors())
        self.state: int = DiscoveringDroid.DISCOVERING


    def unexplored_neighbors(self) -> list[Coord]:
        """Return the unexplored neighbors of the current position."""
        candidates = [self.next_pos(dir) for dir in [NORTH, SOUTH, WEST, EAST]]
        return [nb for  nb in candidates if self.maze[nb] == UNEXPLORED]

    def move(self) -> int:
        """Discover or backtrack the maze."""
        if not self.unexplored:
            return 0

        unexplored = self.unexplored_neighbors()
        if unexplored:
            self.state = DiscoveringDroid.DISCOVERING
            self.unexplored.update(unexplored)
            # print('want to move to', unexplored[0])
            self.current_dir = self.direction(self.current_pos, unexplored[0])
            return self.current_dir

        return self.backtrack()


    def backtrack(self) -> int:
        """Backtrack the maze."""
        if self.unexplored and not self.path:
            raise ValueError('Unexplored but no path')

        if not self.path:
            return 0

        self.state = DiscoveringDroid.BACKTRACKING
        self.current_dir = self.direction(self.current_pos, self.path.pop())
        return self.current_dir

    def status(self, value:int) -> None:
        next_pos = self.next_pos()
        self.unexplored.discard(next_pos)

        if self.state == DiscoveringDroid.BACKTRACKING:
            self.current_pos = next_pos
            return

        self.maze[next_pos] = value
        if value == WALL: return

        self.path.append(self.current_pos)
        self.current_pos = next_pos

        if value == FOUND:
            self.oxygen_pos = self.current_pos


    def direction(self, start: Coord, end: Coord) -> int: 
        if start == end:
            raise ValueError('Same position')
        if (abs(start[0] - end[0]) + abs(start[1] - end[1])) > 1:
            raise ValueError(f'{end} is more than one step away from {start}')

        if start[0] == end[0]:
            return NORTH if start[1] < end[1] else SOUTH

        if start[1] == end[1]:
            return EAST if start[0] < end[0] else WEST

        raise ValueError('Invalid direction')




@perf_timer
def computer(program: defaultdict[int,int], droid:Droid):
    """Computer."""
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    ADJUST_RELATIVE_BASE = 9
    HALT = 99

    MODE_POSITION = 0
    MODE_IMMEDIATE = 1
    MODE_RELATIVE = 2

    modes = [0,0,0,0]

    output:list[int] = []

    relative_base = 0

    def write(instruction_pointer:int, parameter:int, value:int):
        # parameters that an instruction writes to wil never be in immediate mode
        if modes[parameter] == MODE_POSITION:
            program[program[instruction_pointer+parameter]] = value
        elif modes[parameter] == MODE_RELATIVE:
            program[program[instruction_pointer+parameter] + relative_base] = value
        else:
            raise ValueError('Invalid mode for writing {}'.format(modes[parameter]))


    def read(instruction_pointer:int, parameter:int):
        if modes[parameter] == MODE_POSITION:
            return program[program[instruction_pointer + parameter]]
        elif modes[parameter] == MODE_IMMEDIATE:
            return program[instruction_pointer + parameter]
        elif modes[parameter] == MODE_RELATIVE:
            return program[program[instruction_pointer + parameter] + relative_base]
        else:
            raise ValueError('Invalid mode for reading {}'.format(modes[parameter]))

    i = 0
    while i < len(program):
        p3, p2, p1, o1, o2 = list(str(program[i]).zfill(5))
        modes = [0, int(p1), int(p2), int(p3)]
        op = int(o1 + o2)
        if op == HALT:
            break
        elif op == ADD:
            write(i, 3, read(i, 1) + read(i, 2))
            i+=4
        elif op == MULTIPLY:
            write(i, 3, read(i, 1) * read(i, 2))
            i+=4
        elif op == INPUT:
            write(i, 1, droid.move())
            i+=2
        elif op == OUTPUT:
            # output.append(read(i, 1))
            droid.status(read(i, 1))
            i+=2
        elif op == JUMP_IF_TRUE:
            if read(i, 1) != 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == JUMP_IF_FALSE:
            if read(i, 1) == 0:
                i = read(i, 2)
            else:
                i+=3
        elif op == LESS_THAN:
            write(i, 3, 1 if read(i, 1) < read(i, 2) else 0)
            i+=4
        elif op == EQUALS:
            write(i, 3, 1 if read(i, 1) == read(i, 2) else 0)
            i+=4
        elif op == ADJUST_RELATIVE_BASE:
            relative_base += read(i, 1)
            i+=2
        else:
            raise ValueError('Unknown opcode {}'.format(op))

    return output


def part_one(maze: dict[Coord, int], end: Coord) -> int:
    """Solution to part one."""
    q = [(0,0,0)]
    visited:set[Coord] = set()
    while q:
        steps, x, y = heapq.heappop(q)
        if (x,y) == end:
            return steps
        if (x,y) in visited: continue
        visited.add((x,y))
        for dx,dy in wind2dir.values():
            nx,ny = x+dx, y+dy
            if maze[(nx,ny)] == WALL:
                continue
            heapq.heappush(q, (steps+1, nx, ny))

    raise ValueError('No path found')


def part_two(maze: dict[Coord, int], end: Coord) -> int:
    """Solution to part two."""
    q = [(0, *end)]
    visited = set[Coord]()
    mins_to_fill = 0
    while q:
        mins, x, y = heapq.heappop(q)
        if (x,y) not in maze:
            continue
        if maze[(x,y)] == WALL:
            continue
        if (x,y) in visited: continue
        mins_to_fill = max(mins_to_fill, mins)
        visited.add((x,y))
        for dx,dy in wind2dir.values():
            nx,ny = x+dx, y+dy
            heapq.heappush(q, (mins+1, nx, ny))

    
    return mins_to_fill


def main():
    """Parse input file, pass to puzzle solvers."""
    program = list[int]()
    for line in fileinput.input():
        line = line.strip()
        program = list(map(int, line.split(',')))


    # droid = DrunkenDroid()
    droid = DiscoveringDroid()
    computer(defaultdict(int, enumerate(program)), droid)
    print('part_one', part_one(droid.maze, droid.oxygen_pos))
    print('part_two', part_two(droid.maze, droid.oxygen_pos))


if __name__ == '__main__':
    main()
